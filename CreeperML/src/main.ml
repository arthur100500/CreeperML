open CreeperML.Parser_ast
open CreeperML.Parser
open CreeperML.Lexer
open Lexing

let colnum pos = pos.pos_cnum - pos.pos_bol - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "line " ^ l ^ ", column " ^ c

let parse' f s =
  let lexbuf = Lexing.from_string s in
  try f token lexbuf
  with Error ->
    raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))

let parse_program s = parse' parse s
let () = parse_program "let _ = ()" |> ParserAst.show_program |> print_endline
