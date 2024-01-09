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
  let mul a b k = k (a * b)
  let forward x k = k x
  let apply f x = f x
  let add a b k = k (a + b)
  let id x = x
  let () = mul 3 4 (fun x ->
  add x 5 (fun x ->
  forward (apply id x) print_int
  ))
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
  >>= Asm.compile
  |> function
  | Ok x ->
      AsmOptimizer.optimize x
      |> Build.make_exe "./build.sh" " -b \"./bindings.o\""
  | Error x -> print_endline x
