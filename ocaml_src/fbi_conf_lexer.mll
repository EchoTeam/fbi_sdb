{
open Fbi_conf_parser
open Lexing
}

let digit = ['0'-'9']
let space = [' ' '\t' '\r' '\n']
let alpha = ['a'-'z' 'A'-'Z']
let token = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']
let safeQuotation = [^'\\' '"' '\'' '\x00'-'\x1f']+
let safeEscapedChar = [^'\r' '\n' 'r' 'n']

rule token = parse
    | space             { token lexbuf }
    | "#" [^'\n']*      { token lexbuf }
    | "raise"           { RAISE }
    | "lower"           { LOWER }
    | "when"            { WHEN }
    | "flag"            { FLAG }
    | "rate"            { RATE }
    | "latency"         { LATENCY }
    | "value"           { VALUE }
    | "("               { LB }
    | ")"               { RB }
    | "{"               { LPAREN }
    | "}"               { RPAREN }
    | ","               { COMMA }
    | "/"               { SLASH }
    | "*"               { TIMES }
    | "?"               { QMARK }
    | "+"               { PLUS }
    | "-"               { MINUS }
    | "<"               { LESS_THAN }
    | ">"               { GREATER_THAN }
    | "and"             { AND }
    | "or"              { OR }
    | "Miss"            { MISS }
    | "Hits"            { HITS }
    | "using"           { USING }
    | "as"              { AS }
    | digit+ as lxm     { INT (int_of_string lxm) }
    | digit+ "." digit+ as lxm     
                        { FLOAT (float_of_string lxm) }
    | "$" ((alpha | digit) token* as v) 
                        { VAR (v) }
    | alpha token*      { STRING (Lexing.lexeme lexbuf) }
    | '\'' (safeQuotation as s) '\''+  
                        { STRING s }
    | '\''              { STRING (String.concat "" (singleQuotedString lexbuf.lex_curr_pos lexbuf)) }
    | '"' (safeQuotation as s) '"'+  
                        { QSTRING s }
    | '"'               { QSTRING (String.concat "" (doubleQuotedString lexbuf.lex_curr_pos lexbuf)) }
    | eof               { EOF }
  and doubleQuotedString qStart = parse
        | safeQuotation as lxm  { lxm :: doubleQuotedString qStart lexbuf }
        | "\\" 't'              { "\t" :: doubleQuotedString qStart lexbuf }
        | "\\" (safeEscapedChar as c) 
                                { String.make 1 c :: doubleQuotedString qStart lexbuf }
        | '\''                  { "'" :: doubleQuotedString qStart lexbuf }
        | '"'                   { [] }
        | eof                   { raise (Failure "Unterminated quoted string") }
  and singleQuotedString qStart = parse
        | safeQuotation as lxm  { lxm :: singleQuotedString qStart lexbuf }
        | "\\" 't'              { "\t" :: singleQuotedString qStart lexbuf }
        | "\\" (safeEscapedChar as c) 
                                { String.make 1 c :: singleQuotedString qStart lexbuf }
        | '"'                   { "\"" :: singleQuotedString qStart lexbuf }
        | '\''                  { [] }
        | eof                   { raise (Failure "Unterminated quoted string") }
