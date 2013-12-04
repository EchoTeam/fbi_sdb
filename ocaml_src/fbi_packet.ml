(* vim: set ts=4 sts=4 sw=4 et: *)

type packet = {
    counter:        int;
    client_ip:      Unix.inet_addr;
 }
 and outcome = Miss | Hit | Unknown
 and keychain = string list

let client_ip p = p.client_ip

(*
Каждый пакет предваряется заголовком:
    <<(Magic=42):8, (Version=2):8,
        Counter:16, // Счётчик для отброса дублей и подсчёта потерь
        ClientIP:32,    // Кто прислал данные; будет важно при проксировании.
    >>
Дальше идут один или больше кортежей ключей:
    <<Outcome:8, Latency:16, size(Metric):8, Metric/binary, size(KeyChain):8, KeyChain/binary>>
Конструкция KeyChain производится на клиенте и может состоять только из алфавитно-цифровых символов и знаков точки.
Если в fbi:submit() приходит список типа [update, wapo, ViewId], то компоненты, которые 
a) не алфавитно-цифровые атомы, строки, или 
b) бинарники типа ViewId, то они превращаются в Hexadecimal значения:
    fbi:submit([update, _ExcKey=wapo,
            _ViewId=<<18,58,188,222,244,86,120,18,58,188,222,244,86,120>>])
    update.wapo.123ABCDEF45678123ABCDEF45678
 *)
let rec packet_parse buf (len, addr) on_measurement =
    let magic = int_of_char buf.[0] in
    let version = int_of_char buf.[1] in
    if magic = 42 && version = 2 && len > 8 then begin
        metrics_parse buf 8 len on_measurement;
        {
            counter = ushort_of_buf buf 2;
            client_ip = (* Do not trust address within a packet if it is like 0.0.0.0 *)
                    (match int_of_char(buf.[4]), addr with
                    | 0, Unix.ADDR_INET (addr, port) -> addr
                    | _ -> let s =
                            string_of_int (int_of_char buf.[4])
                            ^"."^string_of_int (int_of_char buf.[5])
                            ^"."^string_of_int (int_of_char buf.[6])
                            ^"."^string_of_int (int_of_char buf.[7]) in
                            Unix.inet_addr_of_string s);
        }
    end else
        raise Not_found
and metrics_parse buf pos len on_measurement =
    (* <<Type:8, size(Key):8, Key/binary, Params/binary>> *)
    let measurement_type, key, pos' = get_type_and_key buf pos in
    let newpos = on_measurement measurement_type key buf pos' in
    if newpos < len then metrics_parse buf newpos len on_measurement else ()
and get_type_and_key buf pos =
    let mtype = int_of_char buf.[pos] in
    let ksz = int_of_char buf.[pos + 1] in
    let key = String.create ksz in
    String.blit buf (pos + 2) key 0 ksz;
    mtype, key, pos + 2 + ksz
and outcome_of_char c = match int_of_char c with
    | 0 -> Miss
    | 1 -> Hit
    | _ -> Unknown
and ushort_of_buf buf pos =
    (int_of_char (buf.[pos]) lsl 8) + int_of_char (buf.[pos+1])
and float_of_buf buf pos = 
    let i64 = List.fold_left (fun acc i ->
        Int64.add (Int64.shift_left acc 8) (Int64.of_int (int_of_char buf.[pos + i]))
    ) 0L [0;1;2;3;4;5;6;7] in
    Int64.float_of_bits i64
and uint_of_buf buf pos =
    (int_of_char (buf.[pos]) lsl 24) + (int_of_char (buf.[pos+1]) lsl 16)
        + (int_of_char (buf.[pos+2]) lsl 8) + int_of_char (buf.[pos+3])
and keychain_of_string buf pos len value_buf =
    if pos >= len then
        if Buffer.length value_buf = 0
        then [] else [Buffer.contents value_buf]
    else
        let c = buf.[pos] in
        if c = '/' then begin   (* wapo/dev.wapo/<deadbeef> *)
            let value = Buffer.contents value_buf in
            Buffer.reset value_buf;
            value :: keychain_of_string buf (pos+1) len value_buf
        end else begin
            Buffer.add_char value_buf c;
            keychain_of_string buf (pos+1) len value_buf
        end

