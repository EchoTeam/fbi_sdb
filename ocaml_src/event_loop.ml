open Unix
open ErlangTerm
open ErlangPort

let interact_port_once f =
    let transform = function
      | ET_Atom "stop" -> exit 0
      | ET_Atom "ping" -> ET_Atom "pong"
      | term ->
        (* Return {error, {OriginalTerm, exception, string()}} *)
        try f term with exn -> ET_Tuple [
            ET_Atom "error";
            ET_Tuple [
                term;
                ET_Atom "exception";
                ET_String (Printexc.to_string exn)
                ]
            ]
        in
    let term = erlang_port_read Pervasives.stdin in
    let replyterm = transform term in
    erlang_port_write Pervasives.stdout replyterm

let rec repeat_on_exception f = 
    try f () with exn -> repeat_on_exception f

let serve udp_port packet_processor erlang_cmd loop_dt eval =
    let udp_sock = socket PF_INET SOCK_DGRAM 0 in
    let listen_on = ADDR_INET (inet_addr_any, udp_port) in
    bind udp_sock listen_on;
    let buflen = 16000 in
    let buf = String.create buflen in
    let tty = isatty stdin in
    let old_now = ref (Unix.gettimeofday ()) in
    set_binary_mode_in Pervasives.stdin true;
    set_binary_mode_out Pervasives.stdout true;
    (* OPERATE IN BLOCKING MODE! set_nonblock stdin; *)
    flush_all();
    while true do
        let (r, _, _) = repeat_on_exception (fun () -> select [stdin; udp_sock] [] [] loop_dt) in
        let now = Unix.gettimeofday () in
        if List.mem udp_sock r then begin
            try packet_processor now buf (recvfrom udp_sock buf 0 buflen [])
            with _ -> ()
        end;
        if List.mem stdin r then begin
            if tty then exit 0
            else interact_port_once (erlang_cmd now)
        end;
        if (now -. !old_now >= loop_dt) then begin
            old_now := now;
            eval now;
            flush_all ()
        end
    done

