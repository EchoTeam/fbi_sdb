(* vim: set ts=4 sts=4 sw=4 et: *)

open ErlangTerm

exception WrongMetric

type order_by = OrderByAverageRate of (eventrate * timeperiod)
and eventrate = float
and timeperiod = int (* seconds *)

class virtual metrics =
    object
        val mutable updated_ts = 0.0
        method last_updated = updated_ts
        method virtual update: float -> string -> int -> int
        method virtual fetch: float -> Fbi_conf_type.c_metric  -> float
        method virtual to_eterm: float -> erlang_term
        method virtual order: order_by -> int
    end

type evaluator = (Fbi_conf_type.c_metric -> int -> float) -> Fbi_conf_type.eval_results

type metric_value = {
    rate:      Mavg.mavg list;
    latency:   Mavg.mavg list;
}

class activity_metrics =
    let mk () = {
        rate = List.map Mavg.mavg_new [60; 300; 86400];
        latency = List.map Mavg.mavg_new [60; 300; 86400];
    } in
    object
        inherit metrics
        val mutable miss = mk ()
        val mutable hit = mk ()
        method update now buf pos =
            (* parse *)
            (* <<Outcome:8, Seen:16, Latency:16>> *)
            let outcome = Fbi_packet.outcome_of_char (buf.[pos]) in
            let seen = Fbi_packet.ushort_of_buf buf (pos+1) in
            let latency = float_of_int (Fbi_packet.ushort_of_buf buf (pos+3)) in
            (* update metrics *)
            let rats, lats = match outcome with
                | Fbi_packet.Miss -> miss.rate, miss.latency
                | Fbi_packet.Hit -> hit.rate, hit.latency
                | Fbi_packet.Unknown -> [], [] in
            List.iter (fun m -> Mavg.bump_rate m now seen) rats;
            List.iter (fun m -> Mavg.bump_value m now latency) lats;
            updated_ts <- now;
            pos + 5
        method fetch now metric =
            try
                let outcome, avg = match metric with
                    | Fbi_conf_type.Rate x -> x
                    | Fbi_conf_type.Latency x -> x
                    | _ -> raise WrongMetric in
                let mavgs = match outcome with
                    | Fbi_conf_type.Miss -> miss
                    | Fbi_conf_type.Hit -> hit in
                let mavgs, value_f = match metric with
                    | Fbi_conf_type.Rate _ ->
                        mavgs.rate, (fun mavg -> Mavg.current_rate_average mavg now)
                    | Fbi_conf_type.Latency _ ->
                        mavgs.latency, Mavg.value_average
                    | _ -> raise WrongMetric in
                let mavg = try List.find (fun m -> Mavg.period m == avg) mavgs
                           with Not_found -> List.hd mavgs in
                value_f mavg
            with
                WrongMetric -> 0.0
        method to_eterm now =
            let eterm_of_metric_value mv =
                ET_List (List.map (fun (a, ms, f) ->
                    ET_Tuple [ET_Atom a; ET_List (List.map (fun m ->
                        ET_Int (int_of_float (f m))) ms)]
                ) [ ("rate", mv.rate, fun m -> Mavg.current_rate_average m now);
                    ("latency", mv.latency, Mavg.value_average)]) in
            let v = ET_List (List.map (fun (a, mv) ->
                ET_Tuple [ET_Atom a; eterm_of_metric_value mv]
            ) [("miss", miss); ("hit", hit)]) in
            ET_Tuple [ ET_Atom "activity_metrics"; v]
        method order = function
            | OrderByAverageRate (now, periodn) ->
                let f a =
                    int_of_float (Mavg.current_rate_average (List.nth a.rate periodn) now) in
                (f miss) + (f hit)
    end

class value_metrics =
    object
        inherit metrics
        val mutable value = 0.0
        method update now buf pos =
            (* parse *)
            (* <<Value:64>> *)
            let v = Fbi_packet.float_of_buf buf pos in
            value <- v;
            updated_ts <- now;
            pos + 8
        method fetch _now = function
            | Fbi_conf_type.Value -> value
            | _ -> raise WrongMetric
        method to_eterm _ =
            ET_Tuple [ET_Atom "value_metrics"; ET_Float value]
        method order _ = 0
    end

type flag = {
    mutable evaluator: evaluator;
    mutable args: (metrics option) array;
    mutable affected: bool;
    mutable raised: bool;
    mutable last_updated: float;
 }

module StringKeyHT = struct
        type t = string
        let equal = (=)
        let hash = Hashtbl.hash
        end
module FlagsHash = Hashtbl.Make (StringKeyHT)
module AssocHash = Hashtbl.Make (StringKeyHT)

(* All recorded metrics to date *)

(* All created flags to date *)
let flags_hash = FlagsHash.create 30000
(* Flags that were affected during the past 1 second accounting period *)
let affected_flags_hash = FlagsHash.create 1000
(* Flags that have been raised; need to lower them after non-activity *)

let raised_flags_hash = FlagsHash.create 1000

let assoc_hash = AssocHash.create 30000

let get_period_num = function
    | 60 -> 0
    | 300 -> 1
    | 86400 -> 2
    | _ -> raise Not_found

let compare now periodn (_,y) (_,x) =
    let by = OrderByAverageRate (now, periodn) in
    Pervasives.compare (x#order by) (y#order by)

let is_metric_inactive_for now deltaT metric =
    now -. metric#last_updated > float_of_int(deltaT)

let is_orphan_flag flag = Array.fold_left
    (fun acc x -> acc && (x = None)) true flag.args

let on_remove_metric k metric_value =
    try
        let flag_keys_hash = AssocHash.find assoc_hash k in
        let flags_data = Hashtbl.fold (fun fk n acc ->
            try 
                let flag = FlagsHash.find flags_hash fk in
                (fk, n, flag) :: acc
            with Not_found -> acc) flag_keys_hash [] in
        AssocHash.remove assoc_hash k;
        let _, _ = List.fold_left (fun (nf, na) (fk, n, flag) ->
            flag.args.(n) <- None;
            if is_orphan_flag flag then begin
                FlagsHash.remove affected_flags_hash fk;
                FlagsHash.remove flags_hash fk;
                let eks = Fbi_conf.events_of_flag fk in
                List.iter (fun ek ->
                    try
                        let fk_hash = AssocHash.find assoc_hash ek in
                        Hashtbl.remove fk_hash fk;
                        if (Hashtbl.length fk_hash) = 0 then
                            AssocHash.remove assoc_hash ek
                    with Not_found -> ()
                ) eks;
                (nf + 1, na + List.length eks)
            end else (nf, na)
        ) (0, 0) flags_data in
        ()
    with Not_found ->
        ()

let metrics_lru = Lru.create 10 ~on_auto_remove:(Some on_remove_metric)

let resize_metrics new_size = Lru.resize metrics_lru new_size

let clear_metrics () = Lru.empty metrics_lru

let get_metrics key = Lru.find_notouch metrics_lru key
let get_flag key = FlagsHash.find flags_hash key

(* Go over all metrics *)

let fold_metrics f acc = Lru.fold f acc metrics_lru

(* Go over all flags *)
let fold_flags f acc = FlagsHash.fold f flags_hash acc
let iter_flags f     = FlagsHash.iter f flags_hash

(* Go over all raised flags *)
let fold_raised_flags f acc = FlagsHash.fold f raised_flags_hash acc
let iter_raised_flags f     = FlagsHash.iter f raised_flags_hash

(* rate( *, Miss, 60) <=> <metric>(<category>, <outcome>, <average>) *)
let query_metric now args metric nvar =
    match args.(nvar) with
    | Some arg -> arg#fetch now metric
    | None -> 0.0

let evaluate_affected_view f k flag =
    let was_raised = flag.raised in
    flag.affected <- false;
    f query_metric k flag;
    let is_raised = flag.raised in
    if is_raised <> was_raised then begin
        if is_raised then begin
            FlagsHash.replace raised_flags_hash k flag
        end else
            FlagsHash.remove raised_flags_hash k
    end

(* When configuration changes, this function re-configures the metrics structure and
 * re-evaluates it. *)
let reconfigure_flag unreport_f (*eval_f*) key flag =
    if flag.raised then
        unreport_f key flag;    (* Unreport everything currently flagged *)
    FlagsHash.remove affected_flags_hash key; (* Going to be re-set below *)
    FlagsHash.remove raised_flags_hash key;  (* No flags; shouldn't be in this hash *)
    FlagsHash.remove flags_hash key

(* Invoked every second to figure out whether we need to raise the flag for
 * the items that were affected during the previous second. *)
let foreach_affected_flag f =
    FlagsHash.iter (evaluate_affected_view f) affected_flags_hash;
    FlagsHash.clear affected_flags_hash
         
(* Invoked periodically to scan flagged metric to potentially lower the
 * flag after a period of inactivity. *)
         
let foreach_raised_flag f =
    let process_raised k flag acc =
        f query_metric k flag;
        if flag.raised then acc
        else k :: acc
    in
    let to_remove = FlagsHash.fold process_raised raised_flags_hash [] in
    List.iter (fun k -> FlagsHash.remove raised_flags_hash k) to_remove

let foreach_associated_flag key f =
    try
        let flags_for_key_hash = AssocHash.find assoc_hash key in
        Hashtbl.iter f flags_for_key_hash;
        true
    with Not_found -> false

let add_metrics_and_select metrics key pos =
    let flag = FlagsHash.find flags_hash key in
    flag.args.(pos) <- Some metrics;
    flag.affected <- true;
    FlagsHash.replace affected_flags_hash key flag

let assoc_flag_with_event flag_key event_key n =
    let flags_for_key =
        try AssocHash.find assoc_hash event_key
        with Not_found ->
            let tbl = Hashtbl.create 3 in
            AssocHash.replace assoc_hash event_key tbl;
            tbl in
    Hashtbl.replace flags_for_key flag_key n

let create_metrics = function
    | 0 -> new activity_metrics
    | 1 -> new value_metrics

let get_or_create_metrics key measurement_type =
    try
        Lru.find metrics_lru key
    with Not_found ->
        let metrics = create_metrics measurement_type in
        Lru.add metrics_lru key metrics;
        metrics


let on_each_measurement now measurement_type key buf pos =
    let metrics = get_or_create_metrics key measurement_type in
    let newpos = metrics#update now buf pos in
    let has_affected_flags = foreach_associated_flag key (add_metrics_and_select metrics) in
    if not has_affected_flags then begin
        let flags_data = Fbi_conf.flags_of_event key in
        List.iter (fun (flag_key, eval) ->
            let event_keys = Fbi_conf.events_of_flag flag_key in
            let nargs = List.length event_keys in
            let args = Array.create nargs None in
            ignore (List.fold_left (fun n event_key ->
                if event_key = key then
                    args.(n) <- Some metrics;
                assoc_flag_with_event flag_key event_key n;
                n + 1
            ) 0 event_keys);
            let flag = {
                        last_updated = now;
                        affected = true;
                        raised = false;
                        args = args;
                        evaluator = eval ();
                    } in
            FlagsHash.replace flags_hash flag_key flag;
            FlagsHash.replace affected_flags_hash flag_key flag
        ) flags_data
    end;
    newpos
            
let eterm_of_flag now key flag =
    ET_Tuple [
        ET_String key;
        ET_Atom (match flag.raised with | true -> "raised" | _ -> "lowered");
        ET_Atom (match flag.affected with | true -> "affected" | _ -> "unaffected");
        ET_Tuple [ET_Atom "last_updated"; ET_Float flag.last_updated];
        ET_List (Array.to_list (Array.map (function
            | None -> ET_Atom "none"
            | Some x -> x#to_eterm now
        ) flag.args))
    ]
                
let eterm_of_key_stat now metrics =
    metrics#to_eterm now

let eterm_of_table_stats () =
    ET_List [
        ET_Tuple [ET_Atom "metrics"; ET_Int (Lru.length metrics_lru)];
        ET_Tuple [ET_Atom "flags"; ET_Int (FlagsHash.length flags_hash)];
        ET_Tuple [ET_Atom "affected_flags"; ET_Int (FlagsHash.length affected_flags_hash)];
        ET_Tuple [ET_Atom "raised_flags"; ET_Int (FlagsHash.length raised_flags_hash)];
        ET_Tuple [ET_Atom "assocs"; ET_Int (AssocHash.length assoc_hash)]
    ]
                    
let dump_flag now key flag =
    print_string (string_of_erlang_term (eterm_of_flag now key flag))
                        
let dump_storage_stats now =
    Printf.printf "Dump storage stats: %d\n" (FlagsHash.length flags_hash);
    FlagsHash.iter (dump_flag now) flags_hash
                            
let reset_events_to_flags_assoc () =
    AssocHash.clear assoc_hash

let remove_useless_assocs () =
    let to_remove = AssocHash.fold (fun k _ acc ->
        if Lru.mem metrics_lru k
        then acc
        else k :: acc
    ) assoc_hash [] in
    List.iter (fun k -> AssocHash.remove assoc_hash k) to_remove;
    List.length to_remove
    
    

