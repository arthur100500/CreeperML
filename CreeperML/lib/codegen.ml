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
    | Val of llvalue  (** add carry *)

  let contex = global_context ()
  let the_module = create_module contex "CreeperMLBestLenguage"
  let builder = builder contex
  let named_values : (string, funvar) Hashtbl.t = Hashtbl.create 42
  let function_types : (string, lltype) Hashtbl.t = Hashtbl.create 42
  let float_type = float_type contex
  let bool_type = i32_type contex
  let integer_type = i32_type contex
  let string_type = array_type (i8_type contex) 1
  let unit_type = void_type contex
  let int_const = const_int integer_type
  let str_name n = typed_value n |> string_of_int

  let try_find name msg =
    try
      Hashtbl.find named_values name |> function
      | Func (bind, binder) ->
          binder bind |> fun r ->
          Hashtbl.remove named_values name;
          r
      | Val l -> return l
    with Not_found -> error msg

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
    | _ -> pointer_type contex

  let codegen_imm = function
    | ImmLit t ->
        (match typed_value t with
        | LInt n -> const_int integer_type n
        | LFloat n -> const_float float_type n
        | LBool b -> const_int bool_type (if b then 1 else 0)
        | LUnit -> const_pointer_null unit_type
        | LString str ->
            let l = String.length str in
            let str =
              String.mapi (fun i c -> if i = 0 || i = l - 1 then ' ' else c) str
              |> String.trim
            in
            build_global_stringptr str "" builder)
        |> return
    | ImmVal t ->
        let name = str_name t in
        try_find name
          (Printf.sprintf "Can't find function/value at number %s" name)

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
          let n = List.nth args i |> str_name in
          set_value_name n e;
          Hashtbl.add named_values n (Val e))
        (params f)
    else ();
    return f

  let codegen_predef name args =
    let op f args =
      let* lhs = List.hd args |> codegen_imm in
      let* rhs = List.nth args 1 |> codegen_imm in
      f lhs rhs "op" builder |> return
    in
    let bin_op f args =
      let* lhs = List.hd args |> codegen_imm in
      let* rhs = List.nth args 1 |> codegen_imm in
      let i = f lhs rhs "cmptmp" builder in
      build_zext i integer_type "booltmp" builder |> return
    in
    match name with
    | "f1" (* - *) -> op build_sub args
    | "f2" (* + *) -> op build_add args
    | "f3" (* * *) -> op build_mul args
    | "f4" (* / *) -> op build_sdiv args
    | "f5" (* <= *) -> bin_op (build_icmp Icmp.Sle) args
    | "f6" (* < *) -> bin_op (build_icmp Icmp.Slt) args
    | "f7" (* == *) -> bin_op (build_icmp Icmp.Eq) args
    | "f8" (* > *) -> bin_op (build_icmp Icmp.Sgt) args
    | "f9" (* >= *) -> bin_op (build_icmp Icmp.Sge) args
    | "f10" (* -. *) -> op build_fsub args
    | "f11" (* +. *) -> op build_fadd args
    | "f12" (* *. *) -> op build_fmul args
    | "f13" (* /. *) -> op build_fdiv args
    | "f14" (* <=. *) -> bin_op (build_fcmp Fcmp.Ole) args
    | "f15" (* <. *) -> bin_op (build_fcmp Fcmp.Olt) args
    | "f16" (* ==. *) -> bin_op (build_fcmp Fcmp.Oeq) args
    | "f17" (* >. *) -> bin_op (build_fcmp Fcmp.Olt) args
    | "f18" (* >=. *) -> bin_op (build_fcmp Fcmp.Olt) args
    | "f19" ->
        let ft = function_type unit_type [| integer_type |] in
        let f = declare_function "print_int" ft the_module in
        let* arg = List.hd args |> codegen_imm in
        build_call ft f [| arg |] "" builder |> return
    | "f20" ->
        let ft = function_type unit_type [| pointer_type contex |] in
        let f = declare_function "print_string" ft the_module in
        let* arg = List.hd args |> codegen_imm in
        build_call ft f [| arg |] "" builder |> return
    | name -> Printf.sprintf "fail predef ar %s" name |> error

  let rec rez_t = function TyArrow (_, rez) -> rez_t rez | t -> t

  let rec codegen_expr = function
    | AImm imm -> codegen_imm imm
    | ATuple ims ->
        let* es = monadic_map ims codegen_imm >>| Array.of_list in
        let t = Array.map type_of es |> struct_type contex in
        let addr = build_malloc t "tuplemalloc" builder in
        let alloc i e =
          let addr =
            build_gep t addr [| int_const 0; int_const i |] "elem" builder
          in
          ignore (build_store e addr builder)
        in
        Array.iteri alloc es;
        return addr
    | AApply (ImmVal f, args) -> (
        let rez_t, callname =
          typ f |> rez_t |> fun t ->
          (match t with TyGround TUnit -> "" | _ -> "calltmp") |> fun name ->
          (get_type t, name)
        in
        let name = typed_value f |> Printf.sprintf "f%d" in
        match lookup_function name the_module with
        | None -> codegen_predef name args
        | Some f ->
            let params = params f in
            if List.length args != Array.length params then
              error "Count of args and arrnost' of function are not same"
            else
              let args =
                List.map2
                  (fun a p ->
                    match type_of p |> classify_type with
                    | TypeKind.Pointer ->
                        codegen_imm a >>| fun a ->
                        let addr = build_alloca (type_of a) "polytmp" builder in
                        ignore (build_store a addr builder);
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
              let* fun_t =
                try Hashtbl.find function_types name |> return
                with _ -> Printf.sprintf "can't find type of %s" name |> error
              in
              let rz = build_call fun_t f args callname builder in
              (match return_type fun_t |> classify_type with
              | TypeKind.Pointer -> build_load rez_t rz "polyrez" builder
              | _ -> rz)
              |> return)
    | Aite (cond, { lets = then_lets; res = tr }, { lets = else_lets; res = fl })
      ->
        let curr_block = insertion_block builder in
        let fun_block = block_parent curr_block in
        let test_block = append_block contex "test" fun_block in
        let then_block = append_block contex "then" fun_block in
        let else_block = append_block contex "else" fun_block in
        let merge_block = append_block contex "merge" fun_block in

        ignore (build_br test_block builder);
        position_at_end test_block builder;
        let* cond = codegen_imm cond in
        let cond_val = build_icmp Icmp.Eq cond (int_const 0) "cond" builder in
        ignore (build_cond_br cond_val else_block then_block builder);

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
        ignore (build_store then_val addr builder);
        ignore (build_br merge_block builder);

        position_at_end new_else_block builder;
        ignore (build_store else_val addr builder);
        ignore (build_br merge_block builder);

        position_at_end merge_block builder;
        build_load (type_of else_val) addr "ifrez" builder |> return
    | ATupleAccess (ImmVal name, ix) ->
        let n = str_name name in
        let* str = try_find n "Can't find tuple" in
        let t = typ name |> get_type in
        let addr =
          build_gep t str [| int_const 0; int_const ix |] "access" builder
        in
        build_load (Array.get (subtypes t) ix) addr "loadaccesss" builder
        |> return
    | _ -> error "never happen"

  and codegen_local_var { name; e } =
    let name = str_name name in
    let* body = codegen_expr e in
    let alloca = build_alloca (type_of body) (String.cat name "a") builder in
    ignore (build_store body alloca builder);
    let r = build_load (type_of body) alloca (String.cat name "l") builder in
    Hashtbl.add named_values name (Val r);
    return r

  let codegen_anf_binding main = function
    | AnfVal ({ name; e } as bind) -> (
        match typ name with
        | TyGround TUnit ->
            position_at_end main builder;
            let* _ = codegen_expr e in
            return ()
        | _ ->
            Hashtbl.add named_values (str_name name)
              (Func (bind, codegen_local_var));
            return ())
    | AnfFun { name; args; body = { lets; res = body } } -> (
        let* f = codegen_sig name args in
        let bb = append_block contex "entry" f in
        position_at_end bb builder;
        let* _ = monadic_map (List.rev lets) codegen_local_var in
        try
          let* ret_val = codegen_imm body in
          ignore (build_ret ret_val builder);
          return ()
        with _ ->
          delete_function f;
          error "Funciton binding error")

  let codegen_ret_main b =
    position_at_end b builder;
    build_ret (int_const 0) builder

  let codegen_main =
    let dec = function_type integer_type [||] in
    declare_function "main" dec the_module |> append_block contex "entry"

  let top_lvl code =
    let b = codegen_main in
    let* _ = monadic_map (List.rev code) (codegen_anf_binding b) in
    let _ = codegen_ret_main b in
    return ()

  let dmp_code file = print_module file the_module

  let compile code name =
    let output_ll = Printf.sprintf "%s.ll" name in
    let output_opt_ll = Printf.sprintf "%s-opt.ll" name in
    let output_opt_s = Printf.sprintf "%s-opt.s" name in
    ignore
      (match top_lvl code with
      | Error err -> print_endline err
      | _ -> dmp_code output_ll);
    let _ =
      Printf.sprintf "opt -f -S %s -o %s -Oz" output_ll output_opt_ll
      |> Sys.command
    in
    let _ =
      Printf.sprintf "llc --relocation-model=pic %s" output_opt_ll
      |> Sys.command
    in
    let _ =
      Printf.sprintf "gcc %s lib/bindings.c -o %s" output_opt_s name
      |> Sys.command
    in
    let _ = Printf.sprintf "./%s" name |> Sys.command in
    let _ =
      Printf.sprintf "rm %s %s %s %s" name output_ll output_opt_ll output_opt_s
      |> Sys.command
    in
    ()
end
