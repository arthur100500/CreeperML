(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open CreeperML
open Infer.Infer
open Parser_interface.ParserInterface
open Closure.ClosureConvert
open Anf.AnfConvert
open Anf.AnfOptimizations
open Indexed_ast.IndexedTypeAst
open Asm
open Std
module NameMap = Map.Make (String)

(*
   let f a b = let g c d = let h x y = let j z w = a + b + c + d + x + y + z + w in j in h in g
let () = print_int (f 1 2 3 4 5 6 7 8) *)
let input_program =
  {|
  let f a b c = let g x y z = x + a + y + z + b + c in g
  let p1 = f 1 2
  let p2 = p1 3 4
  let p3 = p2 5 6
  let () = print_int p3
|}

let () =
  let ( >>= ) = Result.bind in
  let apply_db_renaming p = Ok (index_of_typed Std.names p) in
  let apply_closure_convert p = Ok (cf_of_index Std.operators p) in
  let apply_anf_convert p = Ok (anf_of_cf p) in
  let apply_anf_optimizations p = Ok (optimize_moves p) in
  let apply_infer p = top_infer Std.typeenv p in
  let apply_parser = from_string in
  apply_parser input_program >>= apply_infer >>= apply_db_renaming
  >>= apply_closure_convert >>= apply_anf_convert >>= apply_anf_optimizations
  |> function
  | Ok x ->
      Pp.PrettyPrinter.print_anf_program false x |> print_endline;
      Asm.compile x |> AsmOptimizer.optimize
      |> Build.make_exe "./build.sh" " -b \"./bindings.o\""
  | Error x -> print_endline x
