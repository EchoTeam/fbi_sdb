%{
open Fbi_conf_type

%}
%token  <string>    STRING
%token  <string>    QSTRING
%token  <int>       INT
%token  <float>     FLOAT
%token  <string>    VAR
%token  OR AND
%token  LB RB LPAREN RPAREN SLASH
%token  FLAG RAISE LOWER WHEN
%token  QMARK TIMES SLASH PLUS MINUS
%token  LESS_THAN GREATER_THAN
%token  RATE LATENCY VALUE COMMA
%token  MISS HITS USING AS
%token  EOF
%left OR
%left AND
%nonassoc LESS_THAN GREATER_THAN
%left PLUS MINUS
%left TIMES SLASH
%start parse
%type <Fbi_conf_type.c_config> parse
%%

parse: parse_nonempty EOF { $1 }

parse_nonempty:
    | statement { [$1] }
    | statement parse_nonempty { $1::$2 }

statement:
    | FLAG flag_header USING using_list RAISE WHEN expr LOWER WHEN expr
      { DefFlag ($2, { events = $4; raise_when = $7; lower_when = $10 }) }

flag_header:
    | key_pattern QSTRING { ($1, Some $2) }
    | key_pattern { ($1, None) }

using_list:
    | key_pattern AS STRING { [($1, $3)] }
    | using_list COMMA key_pattern AS STRING { $1@[($3, $5)] }

key_pattern:
    | key_pattern_elem { [$1] }
    | key_pattern SLASH key_pattern_elem { $1@[$3] }

key_pattern_elem:
    | STRING    { Exact $1 }
    | VAR       { Var $1 }

expr:
    | metric { $1 }
    | LB expr RB { $2 }
    | expr OR expr { Or ($1, $3) }
    | expr AND expr { And ($1, $3) }
    | expr LESS_THAN expr { LessThan ($1, $3) }
    | expr GREATER_THAN expr  { GreaterThan ($1, $3) }
    | expr SLASH expr  { Div ($1, $3) }
    | expr TIMES expr  { Times ($1, $3) }
    | expr PLUS expr  { Plus ($1, $3) }
    | expr MINUS expr  { Minus ($1, $3) }

metric:
    | INT { Const (float_of_int $1) }
    | FLOAT { Const $1 }
    | fetch { Fetch $1 }

fetch:
    | RATE LB STRING COMMA outcome COMMA INT RB { (Rate ($5, $7), $3) }
    | LATENCY LB STRING COMMA outcome COMMA INT RB { (Latency ($5, $7), $3) }
    | VALUE LB STRING RB { (Value, $3) }

outcome:
    | MISS { Miss }
    | HITS { Hit }
