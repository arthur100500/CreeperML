(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Result

(* open CreeperML.Anf_type_ast.AnfTypeAst *)
open CreeperML.Infer.Infer
open CreeperML.Parser_interface.ParserInterface
open CreeperML.Type_ast.TypeAst
open CreeperML.Closureconvert.ClosureConvert
open CreeperML.Closureconvert.ClosureAst

let lr =
  let open CreeperML.Type_ast.InferTypeUtils in
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let bool_const = t_ground t_bool |> with_lvls 0 0 in
  let arr = t_arrow int_const bool_const |> with_lvls 0 0 in
  let lr = t_arrow int_const arr |> with_lvls 0 0 in
  ("<=", lr)

let mi =
  let open CreeperML.Type_ast.InferTypeUtils in
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let mi = t_arrow int_const arr |> with_lvls 0 0 in
  ("-", mi)

let ml =
  let open CreeperML.Type_ast.InferTypeUtils in
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let ml = t_arrow int_const arr |> with_lvls 0 0 in
  ("*", ml)

let typed t a : ('a, ty) typed = { value = a; typ = t }

let () =
  let ( >>= ) = Result.bind in

  (* let show_program =
       List.iter (fun l ->
           CreeperML.Type_ast.TypeAst.show_typ_let_binding l |> Printf.printf "%s")
     in *)
  let input_program =
    (* {|let rec fac n = if lor n 0 then 1 else n * fac (n - 1)|} *)
    {|let f x = 
        let g y =
          let z = x - y in
          z - 3
        in
        g

      let a = f 10
      let b = a 11
    |}
  in
  let globals =
    [
      typed
        (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TBool))
        "<=";
      typed
        (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
        "-";
      typed
        (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
        "*";
    ]
  in
  let apply_closure_convert p = Ok (cf_program p globals) in
  let apply_infer p = top_infer [ lr; mi; ml ] p in
  let apply_parser = from_string in
  (* let apply_anf x = Ok (anf_of_program x) in
     let print_ast x = show_anf_program x |> print_endline in*)
  apply_parser input_program >>= apply_infer >>= apply_closure_convert
  |> function
  | Ok x -> show_cf_program x |> print_endline
  | Error x -> print_endline x
(*| Ok x -> show_program x
  | _ -> ()
  >>= apply_anf |> function
  | Ok x -> print_ast x
  | Error x -> print_endline x*)
