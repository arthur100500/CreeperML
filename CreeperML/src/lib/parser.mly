(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

%{
    open Parser_ast
    open ParserAstUtils

    let build_mul_e_fun ls b =
    match List.rev ls with
    | [] -> failwith "never happen case of nonempty_list"
    | [ hd ] -> e_fun hd b
    | hd :: tl ->
        List.fold_left (fun acc l -> let_body [] acc |> e_fun l) (e_fun hd b) tl
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token <string>NAME 
%token UNDERBAR
%token LET
%token IN
%token REC
%token FUN
%token ARROW
%token RIGHTPARENT
%token LEFTPARENT
%token COMMA
%token EQUALLY

%token <string>HIGHLVLPREDICATE
%token <string>MIDHIGHLVLPREDICATE
%token <string>MIDLVLPREDICATE
%token <string>LOWMIDLVLPREDICATE
%token <string>LOWLVLPREDICATE

%token EOF

%type <ParserAst.program> parse

%start parse

%%

parse : program EOF { $1 }

literal : 
    | INT { l_int $1 }
    | FLOAT { l_float $1 }
    | STRING { l_string $1 }
    | BOOL { l_bool $1 }
    | LEFTPARENT RIGHTPARENT { l_unit }

lvalue : 
    | UNDERBAR { lv_any }
    | LEFTPARENT RIGHTPARENT { lv_unit }
    | NAME { lv_value $1 }
    | LEFTPARENT lv_tuple_body RIGHTPARENT { lv_tuple $2 }

lv_tuple_body : 
    | lvalue COMMA separated_nonempty_list(COMMA, lvalue) { $1 :: $3 }

let_binding : 
    | LET rec_f lvalue EQUALLY let_body { let_binding ~rec_flag:$2 $3 $5 }

rec_f : 
    | REC { rec_f }
    | { norec_f } 

let_body : 
    | list(inner_let_bind) apply { let_body $1 $2 }

inner_let_bind : 
    | let_binding IN { $1 }

apply : 
    | LEFTPARENT apply RIGHTPARENT { $2 }
    | apply term { e_apply $1 $2 }
    | term { $1 }

term : 
    | LEFTPARENT term RIGHTPARENT { $2 }
    | literal { e_literal $1 }
    | NAME { e_value $1 }
    | LEFTPARENT e_tuple_body RIGHTPARENT { e_tuple $2 }
    | FUN nonempty_list(lvalue) ARROW let_body { build_mul_e_fun $2 $4 }
    | LEFTPARENT predicate RIGHTPARENT { e_value $2 }

predicate : 
    | HIGHLVLPREDICATE { $1 }
    | MIDHIGHLVLPREDICATE { $1 }
    | MIDLVLPREDICATE { $1 }
    | LOWMIDLVLPREDICATE { $1 }
    | LOWLVLPREDICATE { $1 }

e_tuple_body : 
    | apply COMMA separated_nonempty_list(COMMA, apply) { $1 :: $3 }

program : 
    | nonempty_list(let_binding) { $1 }