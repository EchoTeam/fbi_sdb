open Unix
open String
type packet_spec = Activity of (int * int) | Value of float

let make_ev key spec buf = 
    buf.[8] <- char_of_int (match spec with Activity _ -> 0 | Value _ -> 1);    (* type *)
    let kl = String.length key in
    buf.[9] <- char_of_int kl;   (* key length *)
    String.blit key 0 buf 10 kl; (* key *)
    match spec with
        | Activity (outcome, latency) ->
            buf.[10 + kl] <- char_of_int outcome;    (* Positive outcome *)
            buf.[11 + kl] <- char_of_int 0;    (* Number of hits, hi *)
            buf.[12 + kl] <- char_of_int 1;    (* Number of hits, lo *)
            let l_hi, l_lo = if latency > 65535 then (255, 255)
                                else (latency / 255, latency mod 255) in
            buf.[13 + kl] <- char_of_int l_hi;   (* Latency, hi *)
            buf.[14 + kl] <- char_of_int l_lo;   (* Latency, lo *)
            15 + kl
         | Value v ->
            let i64 = Int64.bits_of_float v in
            List.iter (fun n ->
                let b = Int64.logand (Int64.shift_right i64 (8 * n)) 255L in
                buf.[10 + kl + 8 - n - 1] <- char_of_int (Int64.to_int b)
            ) [7;6;5;4;3;2;1;0];
            18 + kl
;;

let random_customer () =
    let n = 100 in
    "customer_long_long"^(string_of_int (Random.int n))

let random_view () =
    (* Trying to get closer to normal distribution *)
    let n = 10 in
    let m = 1000 in
    let v = ref 0 in
    for i = 1 to n do
        v := !v + (Random.int m)
    done;
    (string_of_int !v)^"41234ersfqawer"

(* [(val, weight)] *)
let categories_spec = [
    ("AddItem", 1);
    ("ReplaceItem", 1);
    ("Recreate", 1);
    ("Create", 1);
    ("GetSearch", 1);
    ("GetCounter", 1);
    ("liveSearch", 1);
    ("liveCounter", 1)
]

let categories_weight = List.fold_left (fun aw (k, w) -> aw + w) 0 categories_spec

let random_category () =
    let r = Random.int categories_weight in
    let (Some ctg, _) = List.fold_left (fun (ak, aw) (k, w) ->
        match ak with
        | Some _ -> (ak, aw)
        | None -> 
            let aw' = aw + w in
            if r < aw'
                then (Some k, aw')
                else (None, aw')
    ) (None, 0) categories_spec in
    ctg

let random_key_set () =
    let cust = random_customer () in
    let view = random_view () in
    let ctg = random_category () in
    ["view/"^cust^"/"^view^"/"^ctg;
     "view/"^cust^"/"^view;
     "view/"^cust]

let _ =
    Random.self_init ();
    let cfg_key = ref "view/dev.wapo/<00ff>/AddItem" in
    let cfg_iterations = ref 100000 in
    let cfg_port = ref 1811 in
    Arg.parse (Arg.align [
        ("--key", Arg.String (fun s -> cfg_key := s),
         " Specify key to test with; default \"" ^ !cfg_key ^ "\"");
        ("--iterations", Arg.Int (fun n -> cfg_iterations := n),
         " Number of packets to send; default " ^ string_of_int !cfg_iterations);
        ("--port", Arg.Int (fun n -> cfg_port := n),
         " UDP port to send stuff to; default " ^ string_of_int !cfg_port);
    ]) (fun _ -> ()) "Usage: ";
    let udp_sock = socket PF_INET SOCK_DGRAM 0 in
    let remote = ADDR_INET (inet_addr_loopback, !cfg_port) in
    let buflen = 100 in
    let buf = create buflen in
    buf.[0] <- char_of_int 42;   (* Magic *)
    buf.[1] <- char_of_int 2;    (* version *)
    buf.[2] <- char_of_int 0;    (* counter *)
    buf.[3] <- char_of_int 1;    (* counter *)
    buf.[4] <- char_of_int 127;  (* ipv4 *)
    buf.[5] <- char_of_int 0;    (* ipv4 *)
    buf.[6] <- char_of_int 0;    (* ipv4 *)
    buf.[7] <- char_of_int 1;    (* ipv4 *)
    (* Send 100k of stuff *)
    for i = 1 to !cfg_iterations do
        let keys = random_key_set () in
        (*print_string ((List.hd keys)^"\n");*)
        List.iter (fun key ->
            let sz = make_ev (*!cfg_key*) key (Activity (1, 123)) buf in
            ignore (sendto udp_sock buf 0 sz [] remote)) keys
    done;

    Unix.sleep 1;
    (* Send a packet of death *)
    sendto udp_sock buf 0 8 [] remote

