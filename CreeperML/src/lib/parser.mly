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
%token LET REC IN
%token FUN ARROW
%token LEFTPARENT RIGHTPARENT
%token COMMA
%token EQUALLY

%nonassoc LET
%nonassoc IN
%nonassoc FUN
%left APPLY

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

program : nonempty_list(let_binding) { $1 }

let_binding : LET rec_f lvalue EQUALLY let_body { let_binding ~rec_flag:$2 $3 $5 }

rec_f : 
    | REC { rec_f }
    | { norec_f }

let_body : list(inner_let_bind) expr { let_body $1 $2 }

inner_let_bind : let_binding IN { $1 }

unit : LEFTPARENT RIGHTPARENT { }

lvalue : 
    | UNDERBAR {lv_any }
    | unit { lv_unit }
    | NAME { lv_value $1 }
    | LEFTPARENT lvalue COMMA separated_nonempty_list(COMMA, lvalue) RIGHTPARENT { $2 :: $4 |> lv_tuple }

literal : 
    | INT { l_int $1 }
    | FLOAT { l_float $1 }
    | BOOL { l_bool $1 }
    | STRING { l_string $1 }
    | unit { l_unit }

expr :
    | atom { $1 }
    | FUN nonempty_list(lvalue) ARROW let_body { build_mul_e_fun $2 $4 }
    | apply expr { e_apply $1 $2 }

atom :
    | LEFTPARENT expr RIGHTPARENT { $2 }
    | literal { e_literal $1 }
    | NAME { e_value $1 }
    | LEFTPARENT expr COMMA separated_nonempty_list(COMMA, expr) RIGHTPARENT { $2 :: $4 |> e_tuple }

apply :
    | apply atom { e_apply $1 $2 }
    | atom { $1 }