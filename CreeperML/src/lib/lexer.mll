(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

{
    open Parser
}

let digit = ['0'-'9']
let sign = ['-' '+']

let int_const = sign? digit+
let float_const = sign? digit* '.' digit+
let bool_cosnt = "true" | "false"
let str_const = '"' ['a'-'z' 'A'-'Z' '0'-'9' '_' ' ' '.' ',' ':' ';' '(' ')']* '"'

let name = ['a'-'z' 'A'-'Z' '''] ['a'-'z' 'A'-'Z' ''' '_']*

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
    | whitespace { token lexbuf }
    | int_const { INT (Lexing.lexeme lexbuf |> int_of_string) }
    | float_const { FLOAT (Lexing.lexeme lexbuf |> float_of_string)}
    | bool_cosnt { BOOL (Lexing.lexeme lexbuf |> bool_of_string)}
    | str_const { STRING (Lexing.lexeme lexbuf) }
    | '_' { UNDERBAR }
    | "rec" { REC }
    | "in" { IN }
    | "let" { LET }
    | "fun" { FUN }
    | "->" { ARROW }
    | ',' { COMMA }
    | '=' { EQUALLY }
    | ')' { RIGHTPARENT }
    | '(' { LEFTPARENT }
    | name { NAME (Lexing.lexeme lexbuf) }
    | eof { EOF }
    | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }