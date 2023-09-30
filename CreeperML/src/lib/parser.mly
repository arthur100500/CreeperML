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

parse : p = program; EOF { p }

literal : 
    | n = INT { l_int n }
    | f = FLOAT { l_float f }
    | s = STRING { l_string s }
    | b = BOOL { l_bool b }
    | LEFTPARENT; RIGHTPARENT { l_unit }

lvalue : 
    | UNDERBAR { lv_any }
    | LEFTPARENT; RIGHTPARENT { lv_unit }
    | n = NAME { lv_value n }
    | LEFTPARENT; ts = lv_tuple_body; RIGHTPARENT { lv_tuple ts }

lv_tuple_body : 
    | hd = lvalue; COMMA; tl = separated_nonempty_list(COMMA, lvalue) { hd :: tl }

let_binding : 
    | LET; f = rec_f; lv = lvalue; EQUALLY; b = let_body { let_binding ~rec_flag:f lv b }

rec_f : 
    | REC { rec_f }
    | { norec_f } 

let_body : 
    | ls = list(inner_let_bind); e = expr { let_body ls e }

inner_let_bind : 
    | l = let_binding; IN { l }

expr : 
    | LEFTPARENT; e = expr; RIGHTPARENT { e }
    | e1 = expr; e2 = expr { e_apply e1 e2 }
    | e1 = expr; p = predicate; e2 = expr { e_apply (e_apply p e1) e2 }
    | LEFTPARENT; p = predicate; RIGHTPARENT { p }
    | l = literal { e_literal l }
    | n = NAME { e_value n }
    | FUN; ls = nonempty_list(lvalue); ARROW; b = let_body { build_mul_e_fun ls b }
    | LEFTPARENT; es = e_tuple_body; RIGHTPARENT { e_tuple es }

predicate : 
    | p = HIGHLVLPREDICATE { e_value p }
    | p = MIDHIGHLVLPREDICATE { e_value p }
    | p = MIDLVLPREDICATE { e_value p }
    | p = LOWMIDLVLPREDICATE { e_value p }
    | p = LOWLVLPREDICATE { e_value p }

e_tuple_body : 
    | hd = expr; COMMA; tl = separated_nonempty_list(COMMA, expr) { hd :: tl }

program : 
    | ls = nonempty_list(let_binding) { ls }