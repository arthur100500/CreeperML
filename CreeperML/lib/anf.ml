(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module AnfTypeAst = struct
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Indexed_ast.IndexedTypeAst

  type tlvalue = index_lvalue
  type tliteral = (literal, ty) typed
  type tname = (int, ty) typed
  type imm = ImmVal of tname | ImmLit of tliteral

  type anf_expr =
    | AApply of imm * imm list
    | ATuple of imm list
    | Aite of imm * anf_body * anf_body
    | AImm of imm
    | ATupleAccess of imm * int
    | AClosure of tname * imm list

  and anf_body = { lets : anf_val_binding list; res : imm }
  and anf_val_binding = { name : tname; e : anf_expr }

  type anf_fun_binding = {
    name : tname;
    args : tname list;
    env : tname list;
    body : anf_body;
  }

  type anf_binding = AnfVal of anf_val_binding | AnfFun of anf_fun_binding
  type anf_program = anf_binding list
end

module AnfConvert = struct
  open AnfTypeAst
  open Type_ast.TypeAst
  open Closure.ClosureAst
  open Counter

  (* constructors *)
  let app l r = AApply (l, r)
  let tup l = ATuple l
  let aite i t e = Aite (i, t, e)
  let aval x = AnfVal x
  let imm i = AImm i
  let imml l = ImmLit l
  let immv v = ImmVal v
  let tname t name : tname = { typ = t; value = name }
  let tliteral t literal : tliteral = { typ = t; value = literal }
  let binding name expr : anf_val_binding = { name; e = expr }
  let body lets res : anf_body = { lets; res }
  let tlvalue typ value : tlvalue = { value; typ }

  let rec anf_of_expr (e : cf_typ_expr) =
    match e.value with
    | CFApply (fn, args) ->
        let rec apply_inner fn args =
          match fn.value with
          | CFApply (fn, old_args) -> apply_inner fn (old_args @ args)
          | _ ->
              let fn_bindings, fn_imm = anf_of_expr fn in
              let arg_bindings, arg_imms =
                List.map anf_of_expr args |> fun x ->
                (List.map fst x, List.map snd x) |> fun (x, y) ->
                (List.concat x, y)
              in
              let self_tname = cnt_next () |> tname e.typ in
              let self_binding = app fn_imm arg_imms |> binding self_tname in
              ((self_binding :: arg_bindings) @ fn_bindings, self_tname |> immv)
        in
        apply_inner fn args
    | CFIfElse ite ->
        let i_bindings, if_imm = anf_of_expr ite.cond in
        let t_bindings, then_imm = anf_of_expr ite.t_body in
        let e_bindings, else_imm = anf_of_expr ite.f_body in
        let t_body = body (List.rev t_bindings) then_imm in
        let e_body = body (List.rev e_bindings) else_imm in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding = aite if_imm t_body e_body |> binding self_tname in
        (self_binding :: i_bindings, self_tname |> immv)
    | CFLiteral l -> ([], l |> tliteral e.typ |> imml)
    | CFValue v -> ([], v |> tname e.typ |> immv)
    | CFTuple els ->
        let bindings, tuple_imms = List.map anf_of_expr els |> List.split in
        let bindings = bindings |> List.concat in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding = tup tuple_imms |> binding self_tname in
        (self_binding :: bindings, self_tname |> immv)
    | CFClosure (c, env) ->
        let env = List.map immv env in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding =
          AClosure (c |> tname e.typ, env) |> binding self_tname
        in
        ([ self_binding ], self_tname |> immv)

  let rec lv_binds (lv : tlvalue) (er : imm) : anf_val_binding list =
    match (lv.value, lv.typ) with
    | DLvValue name, _ -> [ binding (tname lv.typ name) (imm er) ]
    | DLvTuple lvalues, TyTuple typs ->
        let inner index (elem, etyp) =
          let t_name = cnt_next () |> tname etyp in
          let access = ATupleAccess (er, index) |> binding t_name in
          let telem = tlvalue etyp elem in
          access :: lv_binds telem (t_name |> immv)
        in
        let zipped = List.map2 (fun x y -> (x, y)) lvalues typs in
        let decs = List.mapi inner zipped in
        List.concat decs
    | DLvUnit, _ -> [ binding (tname lv.typ (cnt_next ())) (imm er) ]
    | _, _ -> []

  let rec anf_of_let_binding (l : cf_typ_let_binding) : anf_val_binding list =
    let reversed_lets = l.cf_body.cf_lets |> List.rev in
    let bindings = List.concat_map anf_of_let_binding reversed_lets in
    let expr_bindings, expr_res = anf_of_expr l.cf_body.cf_expr in
    List.rev (lv_binds l.l_v expr_res) @ expr_bindings @ bindings

  let anf_of_fun_binding (l : cf_fun_let_binding) : anf_fun_binding =
    let reversed_lets = l.b.cf_lets |> List.rev in
    let bindings = List.concat_map anf_of_let_binding reversed_lets in
    let expr_bindings, res = anf_of_expr l.b.cf_expr in
    let inner2 a =
      let arg_name = cnt_next () |> tname a.typ in
      let arg_imm = arg_name |> immv in
      let arg_decs = lv_binds a arg_imm in
      (arg_decs, arg_name)
    in
    let arg_decs, arg_names =
      List.map inner2 l.args |> fun x -> (List.concat_map fst x, List.map snd x)
    in
    let lets = expr_bindings @ bindings @ arg_decs |> List.rev in
    let body = { lets; res } in
    let name = l.name in
    { name; args = arg_names; env = l.env_vars; body }

  let anf_of_cf (p : cf_typ_program) : anf_program =
    let is_val_binding = function AnfFun _ -> false | AnfVal _ -> true in
    let inner = function
      | FunBinding fb -> [ AnfFun (anf_of_fun_binding fb) ]
      | ValBinding vb -> anf_of_let_binding vb |> List.rev |> List.map aval
    in
    List.concat_map inner p |> List.partition is_val_binding
    |> fun (main_statements, function_bindings) ->
    function_bindings @ main_statements
end

module AnfOptimizations = struct
  open AnfTypeAst

  let optimize_moves (p : anf_program) = p
end
