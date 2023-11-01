(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Result
open CreeperML
open Infer.Infer
open Parser_interface.ParserInterface
open Type_ast.TypeAst
open Type_ast.InferTypeUtils
open Closure.ClosureConvert
open Anf.AnfConvert
open Anf.AnfOptimizations
open Counter.Counter
open Db.DbTypeAst
open Pp.PrettyPrinter
module NameMap = Map.Make (String)

let pi =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let unit_const = t_ground t_unit |> with_lvls 0 0 in
  let pi = t_arrow int_const unit_const |> with_lvls 0 0 in
  ("print_int", pi)

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

let pl =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let pl = t_arrow int_const arr |> with_lvls 0 0 in
  ("+", pl)

let typed t a : ('a, ty) typed = { value = a; typ = t }

let nm =
  let nm = NameMap.empty in
  let nm = NameMap.add "<=" (cnt_next ()) nm in
  let nm = NameMap.add "-" (cnt_next ()) nm in
  let nm = NameMap.add "*" (cnt_next ()) nm in
  let nm = NameMap.add "+" (cnt_next ()) nm in
  let nm = NameMap.add "print_int" (cnt_next ()) nm in
  nm

let operators =
  [
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TBool))
    @@ NameMap.find "<=" nm;
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
    @@ NameMap.find "-" nm;
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
    @@ NameMap.find "*" nm;
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
    @@ NameMap.find "+" nm;
    typed (TyArrow (TyGround TInt, TyGround TUnit))
    @@ NameMap.find "print_int" nm;
  ]

let input_program =
  {|
let fac n =
let rec helper n acc =
if n <= 1 then 
acc
else
helper (n - 1) (n * acc)
in
helper n 1
|}

let () =
  let ( >>= ) = Result.bind in
  let apply_db_renaming p = Ok (db_of_typed nm p) in
  let apply_closure_convert p = Ok (cf_of_db operators p) in
  let apply_anf_convert p = Ok (anf_of_cf p) in
  let apply_anf_optimizations p = Ok (optimize_moves p) in
  let apply_infer p = top_infer [ lr; mi; ml; pl; pi ] p in
  let apply_parser = from_string in
  apply_parser input_program >>= apply_infer >>= apply_db_renaming
  >>= apply_closure_convert >>= apply_anf_convert >>= apply_anf_optimizations
  |> function
  | Ok x -> print_anf_program true x |> print_endline
  | Error x -> print_endline x
