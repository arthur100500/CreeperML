(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Anf.AnfTypeAst
open Type_ast.TypeAst
open Type_ast.TypeAstUtils
open Parser_ast.ParserAst
open Monad.Result

let contex = global_context ()
let the_module = create_module contex "main"
let builder = builder contex
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 42
let float_type = float_type contex
let bool_type = i1_type contex
let integer_type = i32_type contex
let string_type = array_type (i8_type contex) 1 (* TODO ?*)

let codegen_imm = function
  | ImmLit t -> (
      match typed_value t with
      | LInt n -> const_int integer_type n |> return
      | LFloat n -> const_float float_type n |> return
      | LBool b -> const_int bool_type (if b then 1 else 0) |> return
      | LString str -> const_string contex str |> return
      | LUnit -> error "unit llvm")
  | ImmVal t -> (
      try typed_value t |> string_of_int |> Hashtbl.find named_values |> return
      with Not_found ->
        typed_value t
        |> Printf.sprintf "Can't find function at number %d"
        |> error)

let codegen_sig name args =
  let rec get_type = function
    | TyGround gr -> (
        match gr with
        | TInt -> return integer_type
        | TString -> return string_type
        | TFloat -> return float_type
        | TBool -> return bool_type
        | TUnit -> error "TODO Unit type")
    | TyTuple ts ->
        monadic_map ts get_type >>| Array.of_list >>| struct_type contex
    | TyArrow (arg, rez) ->
        let* rez = get_type rez in
        let* arg = get_type arg in
        function_type rez (Array.make 1 arg) |> return
    | _ -> error "never happen i guess // or no poly"
    (* TODO make tuples function etc *)
  in
  let* ts = monadic_map args (fun e -> typ e |> get_type) >>| Array.of_list in
  let* ft = typ name |> get_type >>| fun ft -> function_type ft ts in
  let* f =
    match lookup_function (typed_value name |> string_of_int) the_module with
    | None ->
        declare_function (typed_value name |> string_of_int) ft the_module
        |> return
    | Some f ->
        if block_begin f <> At_end f then error "redefinition of function"
        else if element_type (type_of f) <> ft then
          error "redefinition of function"
        else return f
  in
  Array.iteri
    (fun i e ->
      let n = List.nth args i |> typed_value |> string_of_int in
      set_value_name n e;
      Hashtbl.add named_values n e)
    (params f);
  return f

let codegen_predef name args =
  match name with
  | "4" (* + *) ->
      let* lhs = List.hd args |> codegen_imm in
      let* rhs = List.nth args 1 |> codegen_imm in
      build_add lhs rhs "addtmp" builder |> return
  | _ -> error "fail predef"

let rec codegen_expr = function
  | AImm imm -> codegen_imm imm
  | ATuple ims ->
      (* TODO adds to table this structure *)
      monadic_map ims codegen_imm >>| Array.of_list >>| const_struct contex
  | AApply (ImmLit _, _) -> error "Why we have this situation? Never happen"
  | AApply (ImmVal f, args) -> (
      let name = typed_value f |> string_of_int in
      match lookup_function name the_module with
      | Some f ->
          let params = params f in
          if List.length args == Array.length params then
            let* args = monadic_map args codegen_imm >>| Array.of_list in
            build_call (type_of f) f args "calltmp" builder |> return
          else error "Count of args and arrnost' of function are not same"
      | None -> codegen_predef name args)
  | Aite (cond, tr, fl) ->
      let { lets; res = tr } = tr in
      let* _ = monadic_map lets codegen_anf_val_binding in
      let { lets; res = fl } = fl in
      let* _ = monadic_map lets codegen_anf_val_binding in
      (* TODO check lets gen *)
      let* cond = codegen_imm cond in
      let zero = const_int bool_type 0 in
      let cond_val = build_icmp Icmp.Ne cond zero "ifcond" builder in
      let start_bb = insertion_block builder in
      let f = block_parent start_bb in
      let then_bb = append_block contex "then" f in
      position_at_end then_bb builder;
      let* then_val = codegen_imm tr in
      let new_then_bb = insertion_block builder in
      let else_bb = append_block contex "else" f in
      position_at_end else_bb builder;
      let* else_val = codegen_imm fl in
      let new_else_bb = insertion_block builder in
      let merge_bb = append_block contex "ifcont" f in
      position_at_end merge_bb builder;
      let incoming = [ (then_val, new_then_bb); (else_val, new_else_bb) ] in
      let phi = build_phi incoming "iftmp" builder in
      position_at_end start_bb builder;
      ignore (build_cond_br cond_val then_bb else_bb builder);
      position_at_end new_then_bb builder;
      ignore (build_br merge_bb builder);
      position_at_end new_else_bb builder;
      ignore (build_br merge_bb builder);
      position_at_end merge_bb builder;
      return phi
  | ATupleAccess (ImmLit _, _) -> error "Never happen i guess"
  | ATupleAccess (ImmVal name, ix) ->
      let* str =
        try
          typed_value name |> string_of_int |> Hashtbl.find named_values
          |> return
        with Not_found ->
          typed_value name |> Printf.sprintf "Can't find tuple %d" |> error
      in
      build_struct_gep (type_of str) str ix "tmpaccess" builder
      |> return (* TODO check this bullshit *)

and codegen_anf_val_binding { name; e } =
  let* f = codegen_sig name [] in
  let bb = append_block contex "entry" f in
  position_at_end bb builder;
  try
    let* ret_val = codegen_expr e in
    Hashtbl.add named_values (typed_value name |> string_of_int) ret_val;
    let _ = build_ret ret_val builder in
    return f
  with _ ->
    delete_function f;
    error "value binding error"

let rec codegen_anf_binding = function
  | AnfVal bind -> codegen_anf_val_binding bind
  | AnfFun { name; args; body = { lets; res = body } } -> (
      let* f = codegen_sig name args in
      (* need to put lets into f block *)
      let* _ = monadic_map lets codegen_anf_val_binding in
      let bb = append_block contex "entry" f in
      position_at_end bb builder;
      try
        let* ret_val = codegen_imm body in
        let _ = build_ret ret_val builder in
        return f
      with _ ->
        delete_function f;
        error "funciton binding error")
