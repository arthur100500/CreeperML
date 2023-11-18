(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ClosureAst = struct
  open Indexed_ast.IndexedTypeAst
  open Parser_ast.ParserAst
  open Type_ast.TypeAst

  type cf_expr =
    | CFApply of cf_typ_expr * cf_typ_expr list
    | CFLiteral of literal
    | CFValue of int
    | CFClosure of int * (int, ty) typed list
    | CFTuple of cf_typ_expr list
    | CFIfElse of cf_if_else

  and cf_if_else = {
    cond : cf_typ_expr;
    t_body : cf_typ_expr;
    f_body : cf_typ_expr;
  }

  and cf_typ_expr = (cf_expr, ty) typed

  type cf_typ_let_binding = {
    rec_f : rec_flag;
    l_v : index_lvalue;
    cf_body : cf_typ_let_body;
  }

  and cf_typ_let_body = {
    cf_lets : cf_typ_let_binding list;
    cf_expr : cf_typ_expr;
  }

  type cf_fun_let_binding = {
    is_rec : rec_flag;
    name : (int, ty) typed;
    args : index_lvalue list;
    b : cf_typ_let_body;
    env_vars : (int, ty) typed list;
  }

  type cf_binding =
    | FunBinding of cf_fun_let_binding
    | ValBinding of cf_typ_let_binding

  type cf_typ_program = cf_binding list
end

module ClosureConvert = struct
  open ClosureAst
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Indexed_ast.IndexedTypeAst
  open Counter.Counter

  module TypedName = struct
    type t = (int, ty) typed

    let compare (n1 : t) (n2 : t) = compare n1.value n2.value
  end

  module NameSet = Set.Make (TypedName)

  let typed t e : ('a, ty) typed = { value = e; typ = t }

  let rec fst_typ_name_of_lvalue (lval : (ilvalue, ty) typed) =
    match (lval.value, lval.typ) with
    | DLvTuple xs, TyTuple typs ->
        List.hd xs |> typed (List.hd typs) |> fst_typ_name_of_lvalue
    | DLvValue v, ty -> typed ty v
    | _, ty -> typed ty (-1)

  let rec typ_names_of_lvalue (lval : (ilvalue, ty) typed) =
    match (lval.value, lval.typ) with
    | DLvTuple vs, TyTuple typs ->
        let e = NameSet.empty in
        let valtyps = List.map2 (fun x y -> (x, y)) vs typs in
        List.fold_left
          (fun xs x ->
            NameSet.union xs @@ typ_names_of_lvalue (typed (snd x) @@ fst x))
          e valtyps
    | DLvValue v, ty -> typed ty v |> NameSet.singleton
    | _, ty -> typed ty (-1) |> NameSet.singleton

  let rec collect_unbound_variables (f : index_fun_body) global_bindings =
    let rec collect_variables_in_expr (e : index_expr) =
      match e.value with
      | DApply (left, right) ->
          let left = collect_variables_in_expr left in
          let right = collect_variables_in_expr right in
          NameSet.union left right
      | DLiteral _ -> NameSet.empty
      | DValue name -> NameSet.singleton @@ typed e.typ name
      | DTuple exprs ->
          List.fold_left NameSet.union NameSet.empty
            (List.map collect_variables_in_expr exprs)
      | DIfElse ite ->
          let i = collect_variables_in_expr ite.cond in
          let t = collect_variables_in_expr ite.t_body in
          let e = collect_variables_in_expr ite.f_body in
          NameSet.union i t |> NameSet.union e
      | DFun f -> collect_unbound_variables f global_bindings
    in

    let rec collect_variables_in_let (l : index_let_binding) known =
      let e = NameSet.empty in
      let u = NameSet.union in
      let d = NameSet.diff in
      let inner (kn, unk) x =
        let nkn, nukn = collect_variables_in_let x kn in
        (u nkn kn, u nukn unk)
      in
      match l.rec_f with
      | Rec ->
          let known = u known @@ typ_names_of_lvalue l.l_v in
          let iknown, iunknown = List.fold_left inner (known, e) l.body.lets in
          let expr_unknowns = collect_variables_in_expr l.body.expr in
          let expr_unknowns = d expr_unknowns iknown in
          (known, u iunknown expr_unknowns)
      | NoRec ->
          let iknown, iunknown = List.fold_left inner (known, e) l.body.lets in
          let known = u known @@ typ_names_of_lvalue l.l_v in
          let expr_unknowns = collect_variables_in_expr l.body.expr in
          let expr_unknowns = d expr_unknowns iknown in
          (known, u iunknown expr_unknowns)
    in

    let collect_variables_in_body (b : index_let_body) known =
      let e = NameSet.empty in
      let u = NameSet.union in
      let known, unknown =
        List.fold_left
          (fun (kn, unk) x ->
            let new_kn, new_unk = collect_variables_in_let x kn in
            (u kn new_kn, u unk new_unk))
          (known, e) b.lets
      in
      let unknown = NameSet.union unknown @@ collect_variables_in_expr b.expr in
      (known, unknown)
    in

    let known = NameSet.union global_bindings @@ typ_names_of_lvalue f.lvalue in
    let known, unknown = collect_variables_in_body f.b known in
    NameSet.diff unknown known

  let rec closure_free_expr globals r cn (e : index_expr) =
    let cf_expr = closure_free_expr globals r (cnt_next ()) in
    match e.value with
    | DApply (left, right) ->
        let left_decs, left = cf_expr left in
        let right_decs, right = cf_expr right in
        let result = CFApply (left, [ right ]) |> typed e.typ in
        (left_decs @ right_decs, result)
    | DLiteral literal -> ([], CFLiteral literal |> typed e.typ)
    | DValue name -> ([], CFValue name |> typed e.typ)
    | DTuple exprs ->
        let inner xs x =
          let x_decs, x_res = cf_expr x in
          (fst xs @ x_decs, snd xs @ [ x_res ])
        in
        let cf_exprs = List.fold_left inner ([], []) exprs in
        (fst cf_exprs, CFTuple (snd cf_exprs) |> typed e.typ)
    | DIfElse ite ->
        let i_decs, i_expr = cf_expr ite.cond in
        let t_decs, t_expr = cf_expr ite.t_body in
        let e_decs, e_expr = cf_expr ite.f_body in
        let res =
          CFIfElse { cond = i_expr; t_body = t_expr; f_body = e_expr }
        in
        let res_typed = res |> typed e.typ in
        (i_decs @ t_decs @ e_decs, res_typed)
    | DFun f ->
        (* collect fun x -> fun y -> to fun x y -> *)
        let rec convert_fun f args =
          match (f.b.lets, f.b.expr.value) with
          | [], DFun f -> convert_fun f (args @ [ f.lvalue ])
          | _, _ ->
              let bound =
                args
                |> List.map typ_names_of_lvalue
                |> List.fold_left NameSet.union globals
              in
              let unknown_vars = collect_unbound_variables f bound in
              let inner = List.map (cf_let globals r) f.b.lets in
              let expr_closures, cf_expr = cf_expr f.b.expr in
              let inner_cf_lets = List.map snd inner in
              let inner_closures = List.map fst inner in
              let cf_body = { cf_lets = inner_cf_lets; cf_expr } in
              let env = NameSet.to_seq unknown_vars |> List.of_seq in
              let fun_let =
                {
                  is_rec = r;
                  name = typed e.typ cn;
                  args;
                  b = cf_body;
                  env_vars = env;
                }
              in

              let f =
                (if env = [] then CFValue cn else CFClosure (cn, env))
                |> typed e.typ
              in
              (List.concat inner_closures @ expr_closures @ [ fun_let ], f)
        in
        convert_fun f [ f.lvalue ]

  and cf_let (globals : NameSet.t) rec_f (l : index_let_binding) =
    let is_rec =
      match (l.rec_f, rec_f) with NoRec, NoRec -> NoRec | _ -> Rec
    in
    let to_bool = function Rec -> true | _ -> false in
    let globals =
      if to_bool is_rec then NameSet.union globals (typ_names_of_lvalue l.l_v)
      else globals
    in
    let inner = List.map (cf_let globals is_rec) l.body.lets in
    let let_name = fst_typ_name_of_lvalue l.l_v in
    let closures, cf_expr =
      closure_free_expr globals is_rec let_name.value l.body.expr
    in
    let inner_cf_lets = List.map snd inner in
    let inner_closures = List.map fst inner in
    let cf_let_body : cf_typ_let_body = { cf_lets = inner_cf_lets; cf_expr } in
    let rec_f = l.rec_f in
    let l_v = l.l_v in
    let cf_let_binding = { rec_f; l_v; cf_body = cf_let_body } in
    (List.concat inner_closures @ closures, cf_let_binding)

  and cf_of_index globals prog =
    let rec inner g (p : index_program) acc =
      match p with
      | h :: t ->
          let closures, binding = cf_let g h.rec_f h in
          let closures = List.map (fun x -> FunBinding x) closures in
          let binding = ValBinding binding in
          let u = NameSet.union in
          let g = u (typ_names_of_lvalue (h.l_v.value |> typed h.l_v.typ)) g in
          acc @ closures @ [ binding ] |> inner g t
      | [] -> acc
    in
    let set = NameSet.of_list globals in
    inner set prog []
end
