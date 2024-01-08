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
    | Var of anf_val_binding * (anf_val_binding -> llvalue t)
    | Val of llvalue
  (* | Closure of (llvalue * int) *)

  type tmp = TMP | NOTMP

  let contex = global_context ()
  let max_fun = 64
  let the_module = create_module contex "CreeperMLBestLenguage"
  let builder = builder contex
  let named_values : (string, funvar) Hashtbl.t = Hashtbl.create 42
  let function_types : (string, ty) Hashtbl.t = Hashtbl.create 42
  let float_type = float_type contex
  let bool_type = i32_type contex
  let integer_type = i32_type contex
  let string_type = array_type (i8_type contex) 1
  let unit_type = void_type contex
  let ptr = pointer_type contex
  let int_const = const_int integer_type
  let str_name n = typed_value n |> string_of_int
  let last_orig_v = ref (const_null integer_type, false)

  let arity =
    let rec helper = function TyArrow (_, r) -> 1 + helper r | _ -> 0 in
    helper

  let alloc_closure f args arity =
    let argc = Array.length args in
    let argv = args in
    let argv_adr = build_malloc (array_type ptr argc) "arraymalloc" builder in
    let alloc i e =
      let addr =
        build_gep (array_type ptr argc) argv_adr
          [| int_const 0; int_const i |]
          "elem" builder
      in
      ignore (build_store e addr builder)
    in
    Array.iteri alloc argv;
    let arity = int_const arity in
    let alloc_t =
      function_type ptr [| ptr; integer_type; ptr; integer_type; integer_type |]
    in
    let alloc = declare_function "create_function" alloc_t the_module in
    build_call alloc_t alloc
      [| f; argc |> int_const; argv_adr; arity; arity |]
      "closurecreate" builder

  let try_find name msg =
    match Hashtbl.find_opt named_values name with
    | Some v -> (
        match v with
        | Var (bind, binder) ->
            binder bind
            (* >>| fun r ->
               Hashtbl.replace named_values name (Val r);
               r *)
        | Val l ->
            return l
            (* | Closure (f, arity) -> alloc_closure f [||] arity |> return *))
    | None -> error msg

  let try_find_opt name =
    match String.cat "try find opt cant find " name |> try_find name with
    | Ok r -> Some r
    | _ -> None

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
    | _ -> ptr

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
    | ImmVal t -> (
        let name = str_name t in
        match
          (lookup_function (String.cat "f" name) the_module, try_find_opt name)
        with
        | _, Some f -> return f
        | Some f, _ ->
            String.cat "f" name
            |> Hashtbl.find function_types
            |> arity |> alloc_closure f [||] |> return
        | _ ->
            Printf.sprintf "Can't find function/value at number %s" name
            |> error)

  let rec rez_t = function TyArrow (_, rez) -> rez_t rez | t -> t

  let codegen_sig { value = name; typ = t } args =
    let name = Printf.sprintf "f%d" name in
    let ft =
      List.map (fun _ -> ptr) args
      |> Array.of_list
      |> function_type (* rez_t t |> get_type *) ptr
    in
    let* f =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module |> return
      | Some f ->
          if block_begin f <> At_end f then error "redefinition of function"
          else if element_type (type_of f) <> ft then
            error "redefinition of function"
          else return f
    in
    let orig_ft =
      rez_t t |> List.fold_right (fun a acc -> TyArrow (typ a, acc)) args
    in
    let _ = Hashtbl.add function_types name orig_ft in
    let _ =
      List.iter
        (fun a ->
          match typ a with
          | TyArrow _ as t ->
              Hashtbl.add function_types (str_name a |> String.cat "f") t
          | _ -> ())
        args
    in
    Array.iteri
      (fun i e ->
        let a = List.nth args i in
        let n = a |> str_name in
        set_value_name n e;
        (* let to_add =
             match typ a with
             | TyArrow _ -> Closure (e, typ a |> arity)
             | _ -> Val e
           in *)
        Hashtbl.add named_values n (* to_add *) (Val e))
      (params f);
    return f

  let codegen_predef name argv =
    let op_int f argv =
      let lhs = build_load integer_type argv.(0) "opintpoly" builder in
      let rhs = build_load integer_type argv.(1) "opintpoly" builder in
      f lhs rhs "op" builder |> return
    in
    let bin_op_int f argv =
      let lhs = build_load integer_type argv.(0) "binopintpoly" builder in
      let rhs = build_load integer_type argv.(1) "binopintpoly" builder in
      let i = f lhs rhs "cmptmp" builder in
      build_zext i integer_type "booltmp" builder |> return
    in
    let op_float f argv =
      let lhs = build_load float_type argv.(0) "opintpoly" builder in
      let rhs = build_load float_type argv.(1) "opintpoly" builder in
      f lhs rhs "op" builder |> return
    in
    let bin_op_float f argv =
      let lhs = build_load float_type argv.(0) "binopintpoly" builder in
      let rhs = build_load float_type argv.(1) "binopintpoly" builder in
      let i = f lhs rhs "cmptmp" builder in
      build_zext i integer_type "booltmp" builder |> return
    in
    match name with
    | "f1" (* - *) -> op_int build_sub argv
    | "f2" (* + *) -> op_int build_add argv
    | "f3" (* * *) -> op_int build_mul argv
    | "f4" (* / *) -> op_int build_sdiv argv
    | "f5" (* <= *) -> bin_op_int (build_icmp Icmp.Sle) argv
    | "f6" (* < *) -> bin_op_int (build_icmp Icmp.Slt) argv
    | "f7" (* == *) -> bin_op_int (build_icmp Icmp.Eq) argv
    | "f8" (* > *) -> bin_op_int (build_icmp Icmp.Sgt) argv
    | "f9" (* >= *) -> bin_op_int (build_icmp Icmp.Sge) argv
    | "f10" (* -. *) -> op_float build_fsub argv
    | "f11" (* +. *) -> op_float build_fadd argv
    | "f12" (* *. *) -> op_float build_fmul argv
    | "f13" (* /. *) -> op_float build_fdiv argv
    | "f14" (* <=. *) -> bin_op_float (build_fcmp Fcmp.Ole) argv
    | "f15" (* <. *) -> bin_op_float (build_fcmp Fcmp.Olt) argv
    | "f16" (* ==. *) -> bin_op_float (build_fcmp Fcmp.Oeq) argv
    | "f17" (* >. *) -> bin_op_float (build_fcmp Fcmp.Olt) argv
    | "f18" (* >=. *) -> bin_op_float (build_fcmp Fcmp.Olt) argv
    | "f19" ->
        let ft = function_type unit_type [| integer_type |] in
        let f = declare_function "print_int" ft the_module in
        let arg = build_load integer_type argv.(0) "printintargpoly" builder in
        build_call ft f [| arg |] "" builder |> return
    | "f20" ->
        let ft = function_type unit_type [| ptr |] in
        let f = declare_function "print_string_raw" ft the_module in
        build_call ft f [| argv.(0) |] "" builder |> return
    | name -> Printf.sprintf "fail predef ar %s" name |> error

  let rec codegen_expr =
    let apply_to_closure f cl args tmp =
      let argc = List.length args in
      let* argv =
        let* argv = monadic_map args codegen_imm in
        List.map
          (fun a ->
            type_of a |> classify_type |> function
            | TypeKind.Pointer -> a
            | _ ->
                let addr = build_alloca (type_of a) "polytmp" builder in
                ignore (build_store a addr builder);
                addr)
          argv
        |> Array.of_list |> return
      in
      let argv_adr = build_malloc (array_type ptr argc) "arraymalloc" builder in
      let alloc i e =
        let addr =
          build_gep (array_type ptr argc) argv_adr
            [| int_const 0; int_const i |]
            "elem" builder
        in
        ignore (build_store e addr builder)
      in
      Array.iteri alloc argv;
      let apply_t = function_type ptr [| ptr; integer_type; ptr |] in
      let apply = declare_function "apply_args" apply_t the_module in
      let rz =
        build_call apply_t apply
          [| cl; int_const argc; argv_adr |]
          "applyclosure" builder
      in
      ignore (last_orig_v := (rz, true));
      let rz =
        match (typ f |> rez_t, tmp) with
        | TyGround TUnit, _ -> rz
        | t, TMP ->
            let new_addr = build_alloca (get_type t) "tm1000pnewaddr" builder in
            let data = build_load (get_type t) rz "tm1000ptmp" builder in
            ignore (build_store data new_addr builder);
            new_addr
        | _ -> rz
      in
      return rz
    in
    function
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
    | AApply (ImmVal f, args) ->
        let* cl = codegen_imm (ImmVal f) in
        apply_to_closure f cl args TMP
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
        let* cond =
          codegen_imm cond >>| fun c ->
          match type_of c |> classify_type with
          | TypeKind.Pointer -> build_load integer_type c "condload" builder
          | _ -> c
        in
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
        let rz = build_load (type_of else_val) addr "ifrez" builder in
        ignore (last_orig_v := (rz, true));
        return rz
    | ATupleAccess (ImmVal name, ix) ->
        let n = str_name name in
        let* str = try_find n "Can't find tuple" in
        let t = typ name |> get_type in
        let addr =
          build_gep t str [| int_const 0; int_const ix |] "access" builder
        in
        build_load (Array.get (subtypes t) ix) addr "loadaccesss" builder
        |> return
    | AClosure (f, args) ->
        let* cl = codegen_imm (ImmVal f) in
        apply_to_closure f cl args NOTMP
    | _ -> error "never happen"

  and codegen_local_var { name; e } =
    let name = str_name name in
    let* body = codegen_expr e in
    let t = type_of body in
    let r =
      match t |> classify_type with
      | TypeKind.Pointer -> body
      | _ ->
          let addr = build_alloca t (String.cat name "a") builder in
          ignore (build_store body addr builder);
          build_load t addr (String.cat name "l") builder
    in
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
              (Var (bind, codegen_local_var));
            return ())
    | AnfFun { name; args; env; body = { lets; res = body } } -> (
        let* f = env @ args |> codegen_sig name in
        let bb = append_block contex "entry" f in
        position_at_end bb builder;
        let* _ = monadic_map (List.rev lets) codegen_local_var in
        try
          let* v = codegen_imm body in
          let ret_val = if snd !last_orig_v then fst !last_orig_v else v in
          ignore (last_orig_v := (const_null integer_type, false));
          let ret_val =
            match (typ name |> rez_t, type_of ret_val |> classify_type) with
            | TyGround TUnit, _ -> const_pointer_null ptr
            | _, TypeKind.Pointer -> ret_val
            | _ ->
                let addr =
                  build_alloca (type_of ret_val) "polyrezzzz" builder
                in
                ignore (build_store ret_val addr builder);
                addr
          in
          ignore (build_ret ret_val builder);
          return ()
        with _ ->
          delete_function f;
          error "Funciton binding error")

  let codegen_predef_function op =
    let number = str_name op |> int_of_string in
    let args =
      let rec helper = function
        | TyArrow (l, r) ->
            let tl, n = helper l in
            (with_typ r (n + (-1 * number * max_fun)) :: tl, n - 1)
        | _ -> ([], -1)
      in
      helper (typ op) |> fst |> List.rev
    in
    let* f = codegen_sig op args in
    let bb = append_block contex "entry" f in
    position_at_end bb builder;
    let* argv =
      monadic_map args (fun a -> try_find (str_name a) "") >>| Array.of_list
    in
    let* ret_val = codegen_predef (str_name op |> String.cat "f") argv in
    let ret_val =
      match (typ op |> rez_t, type_of ret_val |> classify_type) with
      | TyGround TUnit, _ -> const_pointer_null ptr
      | _, TypeKind.Pointer -> ret_val
      | _ ->
          let addr = build_alloca (type_of ret_val) "polyrezzzz" builder in
          ignore (build_store ret_val addr builder);
          addr
    in
    ignore (build_ret ret_val builder);
    return ()

  let codegen_ret_main b =
    position_at_end b builder;
    build_ret (int_const 0) builder

  let codegen_main =
    let dec = function_type integer_type [||] in
    declare_function "main" dec the_module |> append_block contex "entry"

  let top_lvl code =
    let b = codegen_main in
    let _ =
      List.iter
        (fun { value; typ } ->
          Hashtbl.add function_types (Printf.sprintf "f%d" value) typ)
        Std.Std.operators
    in
    let* _ = monadic_map (List.rev Std.Std.operators) codegen_predef_function in
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
      Printf.sprintf (* "opt -f -S %s -o %s -Oz" *) "cp %s %s" output_ll
        output_opt_ll
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
