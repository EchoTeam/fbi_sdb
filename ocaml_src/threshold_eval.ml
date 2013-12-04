(* vim: set ts=4 sts=4 sw=4 et: *)

open Fbi_storage
open Fbi_conf_type
open ErlangTerm
open ErlangPort

let working_with_a_terminal = Unix.isatty Unix.stdout

type 'a report_type = FlaggedAs of 'a | UnflaggedAs of 'a

let buffer_of_string s =
    let b = Buffer.create (String.length s) in
    Buffer.add_string b s;
    b

let string_of_label = function
    | None -> ""
    | Some s -> " (\""^String.escaped s^"\")"

let etstring_of_label = function
    | None -> ET_String ""
    | Some s -> ET_String s

let eterm_of_arg now = function
    | (None, name) -> ET_Tuple [ET_String name; ET_Atom "noarg"]
    | (Some metrics, name) -> ET_Tuple [ET_String name; metrics#to_eterm now]

let eterm_of_args now args =
    ET_List (List.map (eterm_of_arg now) args)

let report key result now args =
    match result with
    | FlaggedAs (label, expr) when working_with_a_terminal ->
            print_string ("+"^key^string_of_label label^"\n");
            (match expr with None -> () | Some expr ->
                print_string ("  while checking: "^string_of_expr expr^"\n"))
    | UnflaggedAs (label, expr) when working_with_a_terminal ->
            (* There was "+" instead of "-" in the original version. Assuming a
             * bug *)
            print_string ("-"^key^string_of_label label^"\n");
            (match expr with None -> () | Some expr ->
                print_string ("  while checking: "^string_of_expr expr^"\n"))
    | FlaggedAs (label, _) ->
            let reply = ET_Tuple [ET_Atom "flagged";
                                  ET_Binary (buffer_of_string key);
                                  ET_Tuple [etstring_of_label label;
                                            eterm_of_args now args]
                                 ] in
            erlang_port_write stdout reply
    | UnflaggedAs (label, _) ->
            let reply = ET_Tuple [ET_Atom "unflagged";
                                  ET_Binary (buffer_of_string key);
                                  ET_Tuple [etstring_of_label label;
                                            eterm_of_args now args]
                                 ] in
            erlang_port_write stdout reply

let update_flag_add key flag expr label now args =
    flag.raised <- true;
    report key (FlaggedAs (label, expr)) now args

let update_flag_remove key flag expr label now args =
    flag.raised <- false;
    report key (UnflaggedAs (label, expr)) now args

exception BadArg

let zip args names = List.combine (Array.to_list args) names

let evaluate now eval_arg key flag =
    match flag.evaluator (eval_arg now flag.args) with
        | NoChange -> ()
        | Raise (label, expr, arg_names) -> update_flag_add key flag (Some expr) label now (zip flag.args arg_names)
        | Lower (label, expr, arg_names) -> update_flag_remove key flag (Some expr) label now (zip flag.args arg_names)

