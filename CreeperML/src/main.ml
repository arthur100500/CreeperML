(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let () =
  match CreeperML.Parser_interface.from_string {|let (a b, c) = a|} with
  | Ok p -> CreeperML.Parser_ast.ParserAst.show_program p |> print_endline
  | Error err -> print_endline err

(*
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

   let () =
     parse_program {|let a = 1 + 2 ** 3 * 4 - 5|}
     |> ParserAst.show_program |> print_endline
*)
