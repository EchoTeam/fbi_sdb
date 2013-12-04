(* vim: set ts=4 sts=4 sw=4 et: *)

type c_config = c_statement list
 and c_statement = DefFlag of (c_flag_header * c_flag)
 and c_flag_header = (c_pattern * c_label)
 and c_flag = {
            events: (c_pattern * string) list;
            raise_when: c_expr;
            lower_when: c_expr;
        }
 and c_expr =
        | Const of float
        | And of (c_expr * c_expr)
        | Or of (c_expr * c_expr)
        | Minus of (c_expr * c_expr)
        | Plus of (c_expr * c_expr)
        | Times of (c_expr * c_expr)
        | Div of (c_expr * c_expr)
        | LessThan of (c_expr * c_expr)
        | GreaterThan of (c_expr * c_expr)
        | Fetch of (c_metric * string)
 and c_outcome = Miss | Hit
 and c_metric = Rate of (c_outcome * c_averaged) | Latency of (c_outcome * c_averaged) | Value
 and c_averaged = int
 and c_pattern = c_pattern_elem list     (* ["update"; "espn.go"; "*"] *)
 and c_pattern_elem = Exact of string | Var of string
 and c_label = string option (* Free form human readable label *)

let outcome_of_int = function
    | 0 -> Miss
    | 1 -> Hit
    | _ -> Hit

let int_of_outcome = function
    | Miss -> 0
    | Hit -> 1

let string_of_outcome = function
    | Miss -> "Miss"
    | Hit -> "Hits"

let map f = function
    | Const a -> f (Const a)
    | And (a, b) -> f (And (f a, f b))
    | Or (a, b) -> f (Or (f a, f b))
    | Minus (a, b) -> f (Minus (f a, f b))
    | Plus (a, b) -> f (Plus (f a, f b))
    | Times (a, b) -> f (Times (f a, f b))
    | Div (a, b) -> f (Div (f a, f b))
    | LessThan (a, b) -> f (LessThan (f a, f b))
    | GreaterThan (a, b) -> f (GreaterThan (f a, f b))
    | Fetch a -> f (Fetch a)

let iter f expr = ignore (map (fun e -> f e; e) expr)

let rec string_of_expr = function
    | Const v -> string_of_int (int_of_float v)
    | And (a, b) -> "(" ^ string_of_expr a ^ " and " ^ string_of_expr b ^ ")"
    | Or (a, b) -> "(" ^ string_of_expr a ^ " or " ^ string_of_expr b ^ ")"
    | Minus (a, b) -> "(" ^ string_of_expr a ^ " - " ^ string_of_expr b ^ ")"
    | Plus (a, b) -> "(" ^ string_of_expr a ^ " + " ^ string_of_expr b ^ ")"
    | Times (a, b) -> "(" ^ string_of_expr a ^ " * " ^ string_of_expr b ^ ")"
    | Div (a, b) -> "(" ^ string_of_expr a ^ " / " ^ string_of_expr b ^ ")"
    | LessThan (a, b) -> "(" ^ string_of_expr a ^ " < " ^ string_of_expr b ^ ")"
    | GreaterThan (a, b) -> "(" ^ string_of_expr a ^ " > " ^ string_of_expr b ^ ")"
    | Fetch (Rate (o, avg), v) -> "rate("^v^", "^string_of_outcome o^", "^string_of_int avg^")"
    | Fetch (Latency (o, avg), v) -> "latency("^v^", "^string_of_outcome o^", "^string_of_int avg^")"
    | Fetch (Value, v) -> "value("^v^")"

type eval_results =
    NoChange | Raise of c_label * c_expr * string list | Lower of c_label * c_expr * string list

let generate_evaluator (label:string option) (flag:c_flag) (arg_names: string list) =
    let var_names = snd (List.split flag.events) in
    let narg_of_var name =
        let _, narg = List.fold_left (fun (n, na) nm ->
            if nm = name
            then (n + 1, n)
            else (n + 1, na)
        ) (0, -1) var_names in
        narg in
    let rec mk = function
        | Const v -> (fun _ -> v)
        | And (a', b') ->
            let a, b = mk a', mk b' in
            fun arg -> if (a arg <> 0.0) && (b arg <> 0.0) then 1.0 else 0.0
        | Or (a', b') ->
            let a, b = mk a', mk b' in
            fun arg -> if (a arg <> 0.0) || (b arg <> 0.0) then 1.0 else 0.0
        | Minus (a', b') ->
            let a, b = mk a', mk b' in
            fun arg -> a arg -. b arg
        | Plus (a', b') ->
            let a, b = mk a', mk b' in
            fun arg -> a arg +. b arg
        | Times (a', b') ->
            let a, b = mk a', mk b' in
            fun arg -> a arg *. b arg
        | Div (a', b') ->
            let a, b = mk a', mk b' in
            fun arg -> a arg /. b arg
        | LessThan (a', b') ->
            let a, b = mk a', mk b' in
            fun arg -> if a arg < b arg then 1.0 else 0.0
        | GreaterThan (a', b') ->
            let a, b = mk a', mk b' in
            fun arg -> if a arg > b arg then 1.0 else 0.0
        | Fetch (metric, var) ->
            let narg = narg_of_var var in
            fun qry -> qry metric narg
        in
    let test_raise = mk flag.raise_when in
    let test_lower = mk flag.lower_when in
	fun () ->
        let raised = ref false in
        fun arg ->
            if not !raised then
                if test_raise arg <> 0.0 then
                    (* Prevent flicker misfires if not balanced *)
                    if test_lower arg <> 0.0 then NoChange
                    else (raised := true; Raise (label, flag.raise_when, arg_names))
                else NoChange
            else (* raised; try to lower *)
                if test_lower arg <> 0.0
                then (raised := false; Lower (label, flag.lower_when, arg_names))
                else NoChange
