(* vim: set ts=4 sts=4 sw=4 et: *)

open Fbi_conf_type

type keychain = string list

let keychain_of_key = Str.split (Str.regexp "/")
let key_of_keychain = String.concat "/"

module KeyMap = Map.Make(String)

module type TERMINAL_TYPE = sig type t end

module VarMap = Map.Make(String)

module KNode = functor (TermType : TERMINAL_TYPE) ->
    struct
        type knode = {
                exactmatch : knode KeyMap.t;
                wildcard : knode option;
                terminal : (string array -> TermType.t) option;
            }

        let empty_knode = {
                exactmatch = KeyMap.empty;
                wildcard = None;
                terminal = None;
            }

        let rec k2ks n node vars keychain =
            match keychain, node with
            | key :: rest, {exactmatch = map; wildcard = any} ->
                (try
                    let subnode = KeyMap.find key map in
                    k2ks n subnode vars rest
                with Not_found ->
                    (match any with
                    | Some subnode ->
                        vars.(n) <- key;
                        k2ks (n+1) subnode vars rest
                    | None -> raise Not_found))
            | [], {terminal = Some f} -> f vars
            | _, _ -> raise Not_found

        let rec learn_mapping_ll n map node from_pat term_f =
            match from_pat with
            | [] -> {node with terminal = term_f map node.terminal}
            | (Exact s) :: rest ->
                let subnode = try KeyMap.find s node.exactmatch
                                with Not_found -> empty_knode in
                let new_subnode = learn_mapping_ll n map subnode rest term_f in
                {node with exactmatch = KeyMap.add s new_subnode node.exactmatch}
            | (Var v) :: rest ->
                let subnode = match node.wildcard with
                                | None -> empty_knode
                                | Some wc -> wc in
                let new_map = VarMap.add v n map in
                let new_subnode = learn_mapping_ll (n+1) new_map subnode rest term_f in
                {node with wildcard = Some new_subnode}

        let learn_mapping = learn_mapping_ll 0 VarMap.empty
    end

type subst = SubstVal of string | SubstFunc of (string array -> string)

let foe_term to_pat attached map term =
    let subst = List.map (function
        | Exact s -> SubstVal s
        | Var v ->
            let i = VarMap.find v map in
            SubstFunc (fun arr -> arr.(i))
    ) to_pat in
    let f = (fun arr -> List.map (function
        | SubstVal s -> s
        | SubstFunc f -> f arr) subst, attached) in
    match term with
    | None -> Some (fun arr -> [f arr])
    | Some rest -> Some (fun arr -> (f arr) :: rest arr)

let eof_term to_pat map term =
    let subst = List.map (function
        | Exact s -> SubstVal s
        | Var v ->
            let i = VarMap.find v map in
            SubstFunc (fun arr -> arr.(i))
    ) to_pat in
    let f = (fun arr -> List.map (function
        | SubstVal s -> s
        | SubstFunc f -> f arr) subst) in
    match term with
    | None -> Some (fun arr -> [f arr])
    | Some rest -> Some (fun arr -> (f arr) :: rest arr)

let fe_term eval _ _ = Some (fun _ -> eval)

type eval_generator = unit -> (c_metric -> int -> float) -> eval_results

module FOETerm = struct type t = ((keychain * eval_generator) list) end
module FOEKNode = KNode(FOETerm)

module EOFTerm = struct type t = keychain list end
module EOFKNode = KNode(EOFTerm)

module FETerm = struct type t = eval_generator end
module FEKNode = KNode(FETerm)

type mappings_t = {
    mutable flags_of_event : FOEKNode.knode;
    mutable events_of_flag : EOFKNode.knode;
    mutable find_evaluator : FEKNode.knode;
}

let mappings = {
    flags_of_event = FOEKNode.empty_knode;
    events_of_flag = EOFKNode.empty_knode;
    find_evaluator = FEKNode.empty_knode;
}

let find_evaluator flag_key =
    let flag_kc = keychain_of_key flag_key in
    let n = List.length flag_kc in
    let vars = Array.make n "?" in
    FEKNode.k2ks 0 mappings.find_evaluator vars flag_kc

let flags_of_event event_key =
    try
        let event_kc = keychain_of_key event_key in
        let n = List.length event_kc in
        let vars = Array.make n "?" in
        let flags_data = FOEKNode.k2ks 0 mappings.flags_of_event vars event_kc in
        List.map (fun (flag_kc, eval) -> (key_of_keychain flag_kc, eval)) flags_data
    with Not_found -> []

let events_of_flag flag_key =
    try
        let flag_kc = keychain_of_key flag_key in
        let n = List.length flag_kc in
        let vars = Array.make n "?" in
        let events_data = EOFKNode.k2ks 0 mappings.events_of_flag vars flag_kc in
        List.rev_map (fun event_kc -> key_of_keychain event_kc) events_data (* the order is important! *)
    with Not_found -> []

let string_of_pattern pattern =
    let ss = List.map (function
        | Exact x -> x
        | Var x -> "$"^x
    ) pattern in
    String.concat "/" ss

module StringSet = Set.Make(
    struct
        let compare = Pervasives.compare
        type t = string
    end)

exception ConfigError of string

(*
 * Parsing.
 *)

let parse_file filename =
    let ch = open_in filename in
    let lexbuf = Lexing.from_channel ch in
    let r = try try Fbi_conf_parser.parse Fbi_conf_lexer.token lexbuf
    with Parsing.Parse_error ->
        begin match lexbuf with
        | {Lexing.lex_start_pos = sp; Lexing.lex_curr_pos = cp} when sp == cp ->
            raise (ConfigError "Prematurely terminated configuration")
        | {Lexing.lex_start_pos = sp; Lexing.lex_curr_pos = cp} ->
            let s = String.make (cp-sp) ' ' in
            seek_in ch sp;
            ignore(input ch s 0 (cp-sp));
            raise (ConfigError ("Error near \"" ^ String.escaped s ^ "\""))
        end
    with exc -> begin close_in_noerr ch; raise exc end in
    close_in_noerr ch;
    r

module VarSet = Set.Make (
    struct
        let compare = Pervasives.compare
        type t = string
    end )

let key_vars_of p =
    List.fold_left (fun s -> function
        | Var v -> VarSet.add v s
        | _ -> s
    ) VarSet.empty p

let nkey_vars_of p = VarSet.cardinal (key_vars_of p)

let check_key_vars (DefFlag ((pattern, _), flag_cfg)) =
    if flag_cfg.events = [] then
        raise (ConfigError ("Empty list of events for flag "^(string_of_pattern pattern)));
    let header_vars = key_vars_of pattern in
    let first_event_key_vars = key_vars_of (fst (List.hd flag_cfg.events)) in
    if not (VarSet.equal header_vars first_event_key_vars) then
        raise (ConfigError (
            "Set of the first event key vars MUST exactly match the set of vars of the flag key "
            ^(string_of_pattern pattern)
        ));
    List.iter (fun (p, _) ->
        let vars = key_vars_of p in
        if not( VarSet.subset vars header_vars) then
        raise (ConfigError (
            "Set of an event key vars MUST be subset of the set of vars of the flag key "
            ^(string_of_pattern pattern)
        ));
    ) (List.tl flag_cfg.events)

let check_expr_vars (DefFlag ((pattern, _), flag_cfg)) =
    let vars = List.fold_left (fun s (_, v) -> VarSet.add v s) VarSet.empty flag_cfg.events in
    let rec check = function
        | And (e1, e2) -> check e1; check e2
        | Or (e1, e2) -> check e1; check e2
        | Minus (e1, e2) -> check e1; check e2
        | Plus (e1, e2) -> check e1; check e2
        | Times (e1, e2) -> check e1; check e2
        | Div (e1, e2) -> check e1; check e2
        | LessThan (e1, e2) -> check e1; check e2
        | GreaterThan (e1, e2) -> check e1; check e2
        | Fetch (_, name) ->
            if not (VarSet.mem name vars) then
                raise (ConfigError (
                    "Unknown metric '"^name^"' in the raise/lower expression for the flag "
                    ^(string_of_pattern pattern)
                ))
        | _ -> ()
    in
    check flag_cfg.raise_when;
    check flag_cfg.lower_when

let check_flag_config config =
    check_key_vars config;
    check_expr_vars config

let check_config config =
    List.iter check_flag_config config

let rec read_config filename =
    let config = parse_file filename in
    check_config config;
    mappings.flags_of_event <- FOEKNode.empty_knode;
    mappings.events_of_flag <- EOFKNode.empty_knode;
    mappings.find_evaluator <- FEKNode.empty_knode;
    List.iter (function
        | DefFlag ((pattern, label), flag_cfg) ->
            let fp = pattern in
            let eps, _ = List.split flag_cfg.events in
            (* Pre-setup evaluation function *)
            let arg_names = List.map snd flag_cfg.events in
            let gen_evaluator = generate_evaluator label flag_cfg arg_names in
            List.iter (fun ep ->
                if (nkey_vars_of ep) = (nkey_vars_of fp) then
                    mappings.flags_of_event <- FOEKNode.learn_mapping mappings.flags_of_event ep (foe_term fp gen_evaluator);
                mappings.events_of_flag <- EOFKNode.learn_mapping mappings.events_of_flag fp (eof_term ep);
                mappings.find_evaluator <- FEKNode.learn_mapping mappings.find_evaluator fp (fe_term gen_evaluator);
            ) eps;
    ) config

let regex_of_pattern p =
    let rec pattern2re_parts = function
        | [] -> []
        | ("?"::rest) -> "[^/]*" :: pattern2re_parts rest
        | ("*"::rest) -> ".*" :: pattern2re_parts rest
        | (tok::rest) -> (Str.quote tok) :: pattern2re_parts rest in
    Str.regexp (String.concat "/" (pattern2re_parts p) ^ "$")
