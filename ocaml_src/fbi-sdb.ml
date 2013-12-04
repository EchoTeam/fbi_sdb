(* vim: set ts=4 sts=4 sw=4 et: *)

open ErlangTerm

module AddrHT = struct
        type t = Unix.inet_addr
        let equal = (=)
        let hash = Hashtbl.hash
        end
module AddrHash = Hashtbl.Make (AddrHT)
let client_stats_hash = AddrHash.create 32

let record_client_hit now client_ip =
    let (r, m) = try AddrHash.find client_stats_hash client_ip
            with Not_found -> begin
                let rm = (ref 0, Mavg.mavg_new 300) in
                AddrHash.add client_stats_hash client_ip rm;
                rm
            end in
    incr r;
    Mavg.bump_rate m now 1

let dump_client_stats now client_addr (r, m) =
    print_string (Unix.string_of_inet_addr client_addr);
    print_string ": ";
    print_int (int_of_float (Mavg.current_rate_average m now));
    print_string " per ";
    print_int (Mavg.period m);
    print_string " seconds, total ";
    print_int !r;
    print_string "\n"

let eterm_of_client_stat now client_addr (r, m) =
    ET_Tuple [
        ET_String (Unix.string_of_inet_addr client_addr);
        ET_List [
            ET_Tuple [ET_Atom "rate";
                ET_Int (int_of_float (Mavg.current_rate_average m now))];
            ET_Tuple [ET_Atom "smoothing_period"; ET_Int (Mavg.period m)];
            ET_Tuple [ET_Atom "total"; ET_Int !r]
        ]
    ]

let dump_stats now =
    AddrHash.iter (dump_client_stats now) client_stats_hash

let config_file = ref "fbi.conf"
let exit_on_kill_packet = ref false

let erlang_cmd now = 
    let reload_config filename = 
        (* Read config; unreport flagged; re-evaluate and report flagged *)
        Fbi_conf.read_config filename;
        let unreport key flag = Threshold_eval.report key
                                (Threshold_eval.UnflaggedAs (None, None))
                                now
                                [(None, "")] in
        Fbi_storage.reset_events_to_flags_assoc ();
        Fbi_storage.iter_flags (Fbi_storage.reconfigure_flag unreport) in
    function
    | ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Atom "reload_config"] ->
        (try
            reload_config !config_file;
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Atom "ok"]
        with Fbi_conf.ConfigError s ->
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Tuple [ET_Atom "error"; ET_String s]])
    | ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "reload_config"; ET_String filename]] ->
        (try
            reload_config filename;
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Atom "ok"]
        with Fbi_conf.ConfigError s ->
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Tuple [ET_Atom "error"; ET_String s]])
    | ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "reset"; ET_String path_to_config]] ->
        (try
            reload_config path_to_config;
            Fbi_storage.clear_metrics ();
            AddrHash.iter (fun _ (r, _) -> r := 0) client_stats_hash;
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Atom "ok"]
        with Fbi_conf.ConfigError s ->
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Tuple [ET_Atom "error"; ET_String s]])
    | ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Atom "client_stats"] ->
        let client_stats = AddrHash.fold (fun k v acc ->
            (eterm_of_client_stat now k v) :: acc) client_stats_hash [] in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List client_stats]
    | ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Atom "metric_stats"] ->
        let metric_stats = Fbi_storage.fold_metrics (fun acc k v ->
            ET_Tuple [
                ET_String k;
                (Fbi_storage.eterm_of_key_stat now v)
            ] :: acc) [] in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List metric_stats]
    | ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "groupped_metric_stats";
            ET_String pattern0; ET_Int limit; ET_Atom sort_by_category; ET_List all_categories; ET_Int period]] ->
        (* pattern0 : view/?/?/! where '!' is a 'category' placeholder *)
        let extra_categories = List.fold_left (fun acc a_cat ->
            match a_cat with
            | ET_Atom cat when cat <> sort_by_category -> cat :: acc
            | _ -> acc
        ) [] all_categories in
        let pattern = Str.split (Str.regexp "/") pattern0 in
        let search_re =
            let part2re = function
                | "?" -> "\\([^/]+\\)"
                | "!" -> Str.quote sort_by_category
                | p -> Str.quote p in
            Str.regexp (String.concat "/" (List.map part2re pattern) ^ "$") in
        let repl extra_category =
            let _, ps = List.fold_left (fun (i, acc) -> function
                | "?" -> (i + 1), (("\\"^(string_of_int (i + 1))) :: acc)
                | "!" -> i, (extra_category :: acc)
                | p -> i, (p :: acc)
            ) (0, []) pattern in
            String.concat "/" (List.rev ps) in
        let group_key_repl =
            let _, ps = List.fold_left (fun (i, acc) -> function
                | "?" -> (i + 1), (("\\"^(string_of_int (i + 1))) :: acc)
                | _ -> i, acc
            ) (0, []) pattern in
            String.concat "/" (List.rev ps) in
        let extra_key extra_category =
            let cat_repl = repl extra_category in
            (fun key ->
                ignore (Str.string_match search_re key 0);
                Str.replace_matched cat_repl key) in
        let group_key key =
            ignore (Str.string_match search_re key 0);
            let gk = Str.replace_matched group_key_repl key in
            Str.split (Str.regexp "/") gk in
        let extra_keys_for_key =
            let eks = List.map (fun c -> (c, extra_key c)) extra_categories in
            (fun key -> List.map (fun (c, ek) -> (c, ek key)) eks) in

        (try
            let filtered_metric_stats= Fbi_storage.fold_metrics (fun acc k v ->
                if (Str.string_match search_re k 0) then
                    (k, v) :: acc
                else acc
            ) [] in
            let periodn = Fbi_storage.get_period_num period in
            let sorted_metric_stats = List.fast_sort (Fbi_storage.compare now periodn) filtered_metric_stats in
            let limited_metric_stats = match limit with
                | 0 -> sorted_metric_stats
                | _ -> JSKitCombinators.take limit sorted_metric_stats in
            let extra_stats k =
                let extra_keys = extra_keys_for_key k in
                List.fold_left (fun acc (c, ek) -> 
                    try 
                        (c, Fbi_storage.get_metrics ek) :: acc
                    with Not_found -> acc
                ) [] extra_keys in
            let extended_metric_stats = List.map (fun (k, v) ->
                (group_key k), ((sort_by_category, v) :: extra_stats k)
            ) limited_metric_stats in
            let metric_stats = List.fold_left (fun acc (k, v) ->
                ET_Tuple [
                    ET_String (String.concat "/" k);
                    ET_List (List.map (fun (cat, ms) ->
                        ET_Tuple [
                            ET_Atom cat;
                            (Fbi_storage.eterm_of_key_stat now ms)
                        ]
                    ) v)
                ] :: acc
            ) [] extended_metric_stats in
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List metric_stats]
        with Not_found ->
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List []])
    | ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "flush_stale"; ET_Int delta]] ->
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Tuple [ET_Atom "ok";
            ET_Tuple [ET_Int 0; ET_Int 0; ET_Int 0; ET_Int 0]]]
    (* The clauses below are for debug/tests only *)
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "flags_for_key"; ET_String key]] ->
        let lref = ref [] in
        ignore (Fbi_storage.foreach_associated_flag key (fun fk pos ->
            lref := (ET_Tuple [ET_String fk; ET_Int pos]) :: !lref 
        ));
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List !lref]
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "events_of_flag"; ET_String flag_key]] ->
        let event_keys = Fbi_conf.events_of_flag flag_key in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List (List.map (fun s -> ET_String s) event_keys)]
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "flags_of_event"; ET_String event_key]] ->
        let flag_keys, _ = List.split (Fbi_conf.flags_of_event event_key) in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List (List.map (fun s -> ET_String s) flag_keys)]
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Atom "flag_stats"] ->
        let flags_data = Fbi_storage.fold_flags (fun key flag acc ->
            (Fbi_storage.eterm_of_flag now key flag) :: acc) [] in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List flags_data]
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Atom "table_stats"] ->
        let eterm_of_table_stats = Fbi_storage.eterm_of_table_stats () in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; eterm_of_table_stats]
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Atom "num_orphan_flags"] ->
        let n_orphan = Fbi_storage.fold_flags (fun _key flag acc ->
                if Fbi_storage.is_orphan_flag flag then acc + 1 else acc
            ) 0 in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Int n_orphan]
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Atom "raised_flags"] ->
        let raised_flags = Fbi_storage.fold_raised_flags (fun key flag acc ->
            (Fbi_storage.eterm_of_flag now key flag) :: acc) [] in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_List raised_flags]
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Atom "remove_useless_assocs"] ->
        let removed = Fbi_storage.remove_useless_assocs () in
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Int removed]
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "metrics_by_key"; ET_String key]] ->
        (try
            let metrics = Fbi_storage.get_metrics key in
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; Fbi_storage.eterm_of_key_stat now metrics]
        with Not_found ->
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Atom "not_found"]
        )
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; ET_Tuple [ET_Atom "flag_by_key"; ET_String key]] ->
        (try
            let flag = Fbi_storage.get_flag key in
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; Fbi_storage.eterm_of_flag now key flag]
        with Not_found ->
            ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Atom "not_found"]
        )
    |  ET_Tuple [ET_Tuple [ET_Atom "from"; from]; _] ->
        ET_Tuple [ET_Tuple [ET_Atom "to"; from]; ET_Atom "unknown_command"]
    | _ -> raise (Failure "unknown command")

let process_packet now buf lenAddr =
    match try 
        Some (Fbi_packet.packet_parse buf lenAddr (Fbi_storage.on_each_measurement now))
          with
              | exn when !exit_on_kill_packet -> (
                    let bt_str = Printexc.get_backtrace () in
                    output_string stderr ("Backtrace:"^bt_str^"\n");
                    output_string stderr ((Printexc.to_string exn)^"\n");
                    dump_stats now;
                    exit 0
                )
              | _ -> None (* Running in production, ignore KILL packets *)
    with
      | Some packet ->
            record_client_hit now (Fbi_packet.client_ip packet)
      | None -> ()

let adjust_gc_params mk so =
    Gc.set {(Gc.get ()) with Gc.minor_heap_size = mk * 1024;
                             Gc.space_overhead = so; }

let _ =
    (*Printexc.record_backtrace true;*)
    let cfg_port = ref 1811 in
    let ttl = ref (10 * 365 * 86400) in (* Ten years, OK? *)
    let loop = ref 1000 in
    let kreeval = ref 5 in
    let lru_size = ref 100000 in
    let mk = ref 32 in
    let so = ref 80 in
    Arg.parse (Arg.align [
        ("--ttl", Arg.Int (fun n -> ttl := n),
            " Time to live in seconds; default " ^ string_of_int !ttl);
        ("--loop", Arg.Int (fun n -> loop := n),
            " Loop time in milliseconds (never use values < 100 !); default " ^ string_of_int !loop);
        ("--kreeval", Arg.Int (fun n -> kreeval := n),
            " Number of loops to have a chance to reevaluate raised flags " ^ string_of_int !kreeval);
        ("--lru", Arg.Int (fun n -> lru_size := n),
            " LRU size; default " ^ string_of_int !lru_size);
        ("--gcminor", Arg.Int (fun n -> mk := n),
            " Minor heap size, kilobytes; default " ^ string_of_int !mk);
        ("--gcso", Arg.Int (fun n -> so := n),
            " Space overhead, %; default " ^ string_of_int !so);
        ("--config", Arg.String (fun s -> config_file := s),
            " Configuration file; default \"" ^ !config_file ^ "\"");
        ("--port", Arg.Int (fun n -> cfg_port := n),
            " UDP and TCP port to listen on; default " ^ string_of_int !cfg_port);
        ("--flaky", Arg.Unit (fun _ -> exit_on_kill_packet := true),
            " Dump stats and exit when test-sdb sends a KILL packet")
    ]) (fun _ -> ()) "Usage: ";
    Fbi_storage.resize_metrics !lru_size;
    adjust_gc_params !mk !so;
    let reeval_ts = ref (Unix.gettimeofday ()) in
    let reeval_dt = (float_of_int (!loop * !kreeval)) /. 1000.0 in
    let loop_dt = (float_of_int !loop) /. 1000.0 in
    let ttl_dt = float_of_int !ttl in
    let started_ts = Unix.gettimeofday () in
    let reeval now =
        Fbi_storage.foreach_raised_flag (Threshold_eval.evaluate now) in
    let eval now =
        if now -. started_ts >= ttl_dt then (AddrHash.iter (dump_client_stats now) client_stats_hash; exit 0);
        if now -. !reeval_ts >= reeval_dt then (reeval_ts := now; reeval now);
        Fbi_storage.foreach_affected_flag (Threshold_eval.evaluate now) in
    Fbi_conf.read_config !config_file;
    Event_loop.serve !cfg_port process_packet erlang_cmd loop_dt eval

