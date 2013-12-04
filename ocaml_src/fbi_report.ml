open Unix
open String

let err s =
    output_string Pervasives.stderr (s ^ "\n");
    exit 1

type packet_spec = Activity of (int * int) | Value of float

let metric_key = ref ""
let metric_category = ref "*"
let fbi_server = ref "nfbi0"
let exec_args = ref (None, [])
let verbose = ref false
let arg_outcome = ref None
let arg_latency = ref None
let arg_value = ref None

let capture_exec_args s =
    match !exec_args with
        | (None, []) -> exec_args := (Some s, [])
        | (_, args) -> exec_args := (Some s, args @ [s])

(*
 * Run whatever is specified after --exec, and report elapsed wall clock time.
 *)
let run_and_measure cmd args =
    let start = gettimeofday () in
    let result = system (String.concat " " ([cmd] @ args)) in
    let stop = gettimeofday () in
    let latency = match (stop -. start) *. 1000.0 with
                    | lat when lat < 0.0 -> 0 (* Ignore small glitches *)
                    | lat -> int_of_float lat in
    match result with
        | WEXITED code -> (code, latency)
        | _ -> (127, latency)

let send_packet server key spec =
    let udp_sock = socket PF_INET SOCK_DGRAM 0 in
    let remote = ADDR_INET (inet_addr_loopback, 1811) in
    let buflen = 50 in
    let buf = create buflen in
    buf.[0] <- char_of_int 42;   (* Magic *)
    buf.[1] <- char_of_int 2;    (* version *)
    buf.[2] <- char_of_int 0;    (* counter *)
    buf.[3] <- char_of_int 1;    (* counter *)
    buf.[4] <- char_of_int 127;  (* ipv4 *)
    buf.[5] <- char_of_int 0;    (* ipv4 *)
    buf.[6] <- char_of_int 0;    (* ipv4 *)
    buf.[7] <- char_of_int 1;    (* ipv4 *)
    buf.[8] <- char_of_int (match spec with Activity _ -> 0 | Value _ -> 1);    (* type *)
    let kl = String.length key in
    buf.[9] <- char_of_int kl;   (* key length *)
    String.blit key 0 buf 10 kl; (* key *)
    let sz = match spec with
        | Activity (outcome, latency) ->
            buf.[10 + kl] <- char_of_int outcome;    (* Positive outcome *)
            buf.[11 + kl] <- char_of_int 0;    (* Number of hits, hi *)
            buf.[12 + kl] <- char_of_int 1;    (* Number of hits, lo *)
            let l_hi, l_lo = if latency > 65535 then (255, 255)
                                else (latency / 255, latency mod 255) in
            buf.[13 + kl] <- char_of_int l_hi;   (* Latency, hi *)
            buf.[14 + kl] <- char_of_int l_lo;   (* Latency, lo *)
            if !verbose then output_string Pervasives.stderr
                    ("Sending " ^ key 
                     ^ " with outcome " ^ string_of_int outcome
                     ^ ", latency " ^ string_of_int (l_hi * 256 + l_lo)
                     ^ " ms to " ^ !fbi_server ^ "\n");
            15 + kl
         | Value v ->
            let i64 = Int64.bits_of_float v in
            List.iter (fun n ->
                let b = Int64.logand (Int64.shift_right i64 (8 * n)) 255L in
                buf.[10 + kl + 8 - n - 1] <- char_of_int (Int64.to_int b)
            ) [7;6;5;4;3;2;1;0];
            18 + kl in
    0 < sendto udp_sock buf 0 sz [] remote

(* Validate string characters *)
exception Alphabet of string

let rec main () =
    Arg.parse (Arg.align [
        ("--server", Arg.String (fun s -> fbi_server := s),
            " FBI Server address; default is \"" ^ !fbi_server ^ "\"");
        ("--key", Arg.String (fun s -> metric_key := s),
            " Metric key to send; e.g. \"update/metric/1234\"");
        ("--outcome", Arg.Int (fun n -> arg_outcome := Some n),
            " Specify outcome, e.g. 1 for positive; (incompatible with --exec)");
        ("--latency", Arg.Int (fun lat -> arg_latency := Some lat),
            " Specify latency operation (incompatible with --exec)");
        ("--value", Arg.Float (fun v -> arg_value := Some v),
            " Specify value (incompatible with --exec)");
        ("--exec", Arg.Rest capture_exec_args,
            " Execute given program (with arguments) and report return code.");
        ("--verbose", Arg.Unit (fun s -> verbose := true),
            " Be extra chatty about what we send")
    ]) (fun _ -> ()) "Usage: ";
    validate_metric !metric_key;
    validate_outcome !arg_outcome;
    validate_latency !arg_latency;
    let (exit_code, spec) =
    match !exec_args, !arg_outcome, !arg_latency, !arg_value with
        | (None, _), None, None, None ->
            err "Specify either --exec, or --outcome & --latency."
        | (None, _), Some outcome, Some latency, None -> (0, Activity (outcome, latency))
        | (None, _), None, None, Some value -> (0, Value value)
        | (None, _), _, _, _ ->
            err "Either --value or both --outcome and --latency must be given."
        | (Some cmd, args), None, None, None ->
            let (exit_code, latency) = run_and_measure cmd args in
            let outcome = if exit_code = 0 then 1 else 0 in
            (exit_code, Activity (outcome, latency))
        | (Some cmd, args), _, _, _ ->
            err "--exec is not compatible with --outcome and --latency."
    in
    match (try send_packet !fbi_server !metric_key spec
           with
                | exc when !verbose ->
                    output_string Pervasives.stderr
                        (Printexc.to_string exc ^ "\n");
                    false
                | _ -> false) with
        | false -> err "Cannot send packet to FBI."
        | true -> exit exit_code
and alpha rs s =
    let re = Str.regexp (rs ^ "$") in
    if not (Str.string_match re s 0) then raise (Alphabet rs)
and validate_metric s = match s with
    | "" -> err "Empty key; use --key with non-empty argument."
    | s -> try ignore(String.index s '/'); alpha "[a-z][a-zA-Z0-9/]*" s with
           | Not_found -> err "Expecting slash-separated multi-component key."
           | Alphabet s -> err ("Incompatible characters in --key argument."
                                ^ " Expected " ^ s)
and validate_latency lat = match lat with
    | Some n when n < 0 -> err "Non-negative --latency expected."
    | _ -> ()
and validate_outcome out = match out with
    | Some n when n < 0 -> err "Negative --outcome; expecting 0..255"
    | Some n when n > 255 -> err "Too large --outcome; expected 0..255."
    | _ -> ()

let _ = main ()
