(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Anf.AnfTypeAst
open Type_ast.TypeAst
open Type_ast.TypeAstUtils
open Parser_ast.ParserAst
open Monad.Result

module Codegen = struct
  type funvar =
    | Func of anf_val_binding * (anf_val_binding -> llvalue t)
    | Val of llvalue

  let contex = global_context ()
  let the_module = create_module contex "main"
  let builder = builder contex
  let named_values : (string, funvar) Hashtbl.t = Hashtbl.create 42
  let function_types : (string, lltype) Hashtbl.t = Hashtbl.create 42
  let float_type = float_type contex
  let bool_type = i32_type contex
  let integer_type = i32_type contex
  let string_type = array_type (i8_type contex) 1 (* TODO ?*)
  let unit_type = void_type contex

  let rec get_type = function
    | TyGround gr -> (
        match gr with
        | TInt -> integer_type
        | TString -> string_type
        | TFloat -> float_type
        | TBool -> bool_type
        | TUnit -> unit_type)
    | TyTuple ts -> List.map get_type ts |> Array.of_list |> struct_type contex
    | TyArrow _ as arr ->
        let rec args = function
          | TyArrow (arg, r) -> args r |> fun (ars, r) -> (arg :: ars, r)
          | t -> ([], t)
        in
        let args, r = args arr in
        function_type (get_type r) (List.map get_type args |> Array.of_list)
    | _ -> pointer_type contex (* poly type*)

  let malloc name t = build_malloc (get_type t) name builder

  let codegen_imm = function
    | ImmLit t -> (
        match typed_value t with
        | LInt n -> const_int integer_type n |> return
        | LFloat n -> const_float float_type n |> return
        | LBool b -> const_int bool_type (if b then 1 else 0) |> return
        | LString str -> const_string contex str |> return
        | LUnit -> const_pointer_null unit_type |> return)
    | ImmVal t -> (
        let name = typed_value t |> string_of_int in
        try
          Hashtbl.find named_values name |> function
          | Func (bind, binder) ->
              binder bind |> fun r ->
              Hashtbl.remove named_values name;
              r
          | Val l -> return l
        with Not_found ->
          Printf.sprintf "Can't find function/value at number %s" name |> error)

  let codegen_sig { value = name; typ } args =
    let name = Printf.sprintf "f%d" name in
    let ft = get_type typ in
    let* f =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module |> return
      | Some f ->
          if block_begin f <> At_end f then error "redefinition of function"
          else if element_type (type_of f) <> ft then
            error "redefinition of function"
          else return f
    in
    Hashtbl.add function_types name ft;
    if List.length args > 0 then
      Array.iteri
        (fun i e ->
          let n = List.nth args i |> typed_value |> string_of_int in
          set_value_name n e;
          Hashtbl.add named_values n (Val e))
        (params f)
    else ();
    return f

  let codegen_predef name args =
    match name with
    | "f1" (* <= *) ->
        let* lhs = List.hd args |> codegen_imm in
        let* rhs = List.nth args 1 |> codegen_imm in
        let i = build_icmp Icmp.Sle lhs rhs "cmptmp" builder in
        build_zext i integer_type "booltmp" builder |> return
    | "f2" (* - *) ->
        let* lhs = List.hd args |> codegen_imm in
        let* rhs = List.nth args 1 |> codegen_imm in
        build_sub lhs rhs "subtmp" builder |> return
    | "f3" (* * *) ->
        let* lhs = List.hd args |> codegen_imm in
        let* rhs = List.nth args 1 |> codegen_imm in
        build_mul lhs rhs "multmp" builder |> return
    | "f4" (* + *) ->
        let* lhs = List.hd args |> codegen_imm in
        let* rhs = List.nth args 1 |> codegen_imm in
        build_add lhs rhs "addtmp" builder |> return
    | "f5" ->
        let f =
          declare_function "print_int"
            (function_type (void_type contex) [| integer_type |])
            the_module
        in
        let* arg = List.hd args |> codegen_imm in
        build_call
          (function_type (void_type contex) [| integer_type |])
          f [| arg |] "calltmp" builder
        |> return
    | _ -> error "fail predef"

  let rec rez_t = function TyArrow (_, rez) -> rez_t rez | t -> t

  let rec codegen_expr = function
    | AImm imm -> codegen_imm imm
    | ATuple ims ->
        monadic_map ims codegen_imm
        >>| Array.of_list
        >>| const_struct contex (* mb do as codegentwo *)
    | AApply (ImmLit _, _) -> error "Why we have this situation? Never happen"
    | AApply (ImmVal f, args) -> (
        let rez_t = typ f |> rez_t |> get_type in
        let name = typed_value f |> Printf.sprintf "f%d" in
        match lookup_function name the_module with
        | None -> codegen_predef name args
        | Some f ->
            let params = params f in
            if List.length args == Array.length params then
              let args =
                List.map2
                  (fun a p ->
                    match type_of p |> classify_type with
                    | TypeKind.Pointer ->
                        codegen_imm a >>| fun a ->
                        let addr = build_alloca (type_of a) "polytmp" builder in
                        let _ = build_store a addr builder in
                        addr
                    | _ -> codegen_imm a)
                  args (Array.to_list params)
              in
              let* args =
                List.fold_right
                  (fun a acc ->
                    let* a = a in
                    let* acc = acc in
                    a :: acc |> return)
                  args (return [])
                >>| Array.of_list
              in
              let fun_t = Hashtbl.find function_types name in
              let rz = build_call fun_t f args "calltmp" builder in
              match return_type fun_t |> classify_type with
              | TypeKind.Pointer ->
                  build_load rez_t rz "polyrez" builder |> return
              | _ -> return rz
            else error "Count of args and arrnost' of function are not same")
    | Aite (cond, { lets = then_lets; res = tr }, { lets = else_lets; res = fl })
      ->
        let curr_block = insertion_block builder in
        let fun_block = block_parent curr_block in
        let test_block = append_block contex "test" fun_block in
        let then_block = append_block contex "then" fun_block in
        let else_block = append_block contex "else" fun_block in
        let merge_block = append_block contex "merge" fun_block in

        let _ = build_br test_block builder in
        position_at_end test_block builder;
        let* cond = codegen_imm cond in
        let cond_val =
          build_icmp Icmp.Eq cond (const_int integer_type 0) "cond" builder
        in
        let _ = build_cond_br cond_val else_block then_block builder in

        position_at_end then_block builder;
        let* _ = monadic_map (List.rev then_lets) codegen_local_var in
        let* then_val = codegen_imm tr in
        let new_then_block = insertion_block builder in

        position_at_end else_block builder;
        let* _ = monadic_map (List.rev else_lets) codegen_local_var in
        let* else_val = codegen_imm fl in
        let new_else_block = insertion_block builder in

        let addr =
          insertion_block builder |> block_parent |> entry_block |> instr_begin
          |> builder_at contex
          |> build_alloca (type_of else_val) "ifrezptr"
        in
        position_at_end new_then_block builder;
        let _ = build_store then_val addr builder in
        let _ = build_br merge_block builder in

        position_at_end new_else_block builder;
        let _ = build_store else_val addr builder in
        let _ = build_br merge_block builder in

        position_at_end merge_block builder;
        build_load (type_of else_val) addr "ifrez" builder |> return
    | ATupleAccess (ImmLit _, _) -> error "Never happen i guess"
    | ATupleAccess (ImmVal name, ix) ->
        let* str =
          try
            typed_value name |> string_of_int |> Hashtbl.find named_values
            |> function
            | Func (bind, binder) ->
                binder bind |> fun r ->
                Hashtbl.remove named_values (typed_value name |> string_of_int);
                r
            | Val v -> return v
          with Not_found ->
            typed_value name |> Printf.sprintf "Can't find tuple %d" |> error
        in
        build_struct_gep (type_of str) str ix "tmpaccess" builder |> return

  and codegen_local_var { name; e } =
    let name = typed_value name |> string_of_int in
    let* body = codegen_expr e in
    let alloca = build_alloca (type_of body) name builder in
    let _ = build_store body alloca builder in
    let r = build_load (type_of body) alloca (String.cat "_" name) builder in
    Hashtbl.add named_values name (Val r);
    return r

  let codegen_anf_binding main = function
    | AnfVal ({ name; e } as bind) -> (
        match typ name with
        | TyGround TUnit ->
            position_at_end main builder;
            codegen_expr e >>= fun _ -> return ()
        | _ ->
            Hashtbl.add named_values
              (typed_value name |> string_of_int)
              (Func (bind, codegen_local_var));
            return ())
    | AnfFun { name; args; body = { lets; res = body } } -> (
        let* f = codegen_sig name args in
        let bb = append_block contex "entry" f in
        position_at_end bb builder;
        let* _ = monadic_map (List.rev lets) codegen_local_var in
        try
          let* ret_val = codegen_imm body in
          let _ = build_ret ret_val builder in
          return ()
        with _ ->
          delete_function f;
          error "funciton binding error")

  let codegen_ret_main b =
    position_at_end b builder;
    build_ret (const_int integer_type 0) builder

  let codegen_main =
    let dec = function_type integer_type [||] in
    declare_function "main" dec the_module |> append_block contex "entry"

  let top_lvl code =
    let b = codegen_main in
    let* _ = monadic_map (List.rev code) (codegen_anf_binding b) in
    let _ = codegen_ret_main b in
    return ()

  let dmp_code file = print_module file the_module
end
