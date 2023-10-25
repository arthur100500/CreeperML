(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Result

open CreeperML
open Infer.Infer
open Parser_interface.ParserInterface
open Type_ast.TypeAst
open Type_ast.InferTypeUtils
open Closure.ClosureConvert
open Anf.AnfTypeAst
open Anf.AnfConvert
open Counter.Counter
open Db.DbTypeAst
module NameMap = Map.Make (String)

let lr =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let bool_const = t_ground t_bool |> with_lvls 0 0 in
  let arr = t_arrow int_const bool_const |> with_lvls 0 0 in
  let lr = t_arrow int_const arr |> with_lvls 0 0 in
  ("<=", lr)

let mi =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let mi = t_arrow int_const arr |> with_lvls 0 0 in
  ("-", mi)

let ml =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let ml = t_arrow int_const arr |> with_lvls 0 0 in
  ("*", ml)

let typed t a : ('a, ty) typed = { value = a; typ = t }

let nm =
  let nm = NameMap.empty in
  let nm = NameMap.add "<=" (cnt_next ()) nm in
  let nm = NameMap.add "-" (cnt_next ()) nm in
  let nm = NameMap.add "*" (cnt_next ()) nm in
  nm

let operators =
  [
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TBool))
    @@ NameMap.find "<=" nm;
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
    @@ NameMap.find "-" nm;
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
    @@ NameMap.find "*" nm;
  ]

let input_program =
  {|
let f x =
  let g y = x in g

let a = f 10 11
let b = f 20 22
|}

let () =
  let ( >>= ) = Result.bind in
  let apply_db_renaming p = Ok (db_program_of_typed_program nm p) in
  let apply_closure_convert p = Ok (cf_program p operators) in
  let apply_anf_convert p = Ok (anf_of_program p) in
  let apply_infer p = top_infer [ lr; mi; ml ] p in
  let apply_parser = from_string in
  apply_parser input_program >>= apply_infer >>= apply_db_renaming
  >>= apply_closure_convert >>= apply_anf_convert
  |> function
  | Ok x -> show_anf_program false x |> print_endline
  | Error x -> print_endline x
