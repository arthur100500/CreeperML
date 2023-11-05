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
    body : anf_body;
    env_vars : tname list;
  }

  type anf_binding = AnfVal of anf_val_binding | AnfFun of anf_fun_binding
  type anf_program = anf_binding list
end

module AnfConvert = struct
  open AnfTypeAst
  open Type_ast.TypeAst
  open Closure.ClosureAst
  open Counter.Counter

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

  type aer = anf_val_binding list * imm

  let rec anf_of_expr (e : cf_typ_expr) : aer =
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
              (fn_bindings @ arg_bindings @ [ self_binding ], self_tname |> immv)
        in
        apply_inner fn args
    | CFIfElse ite ->
        let i_bindings, if_imm = anf_of_expr ite.cond in
        let t_bindings, then_imm = anf_of_expr ite.t_body in
        let e_bindings, else_imm = anf_of_expr ite.f_body in
        let t_body = body t_bindings then_imm in
        let e_body = body e_bindings else_imm in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding = aite if_imm t_body e_body |> binding self_tname in
        (i_bindings @ [ self_binding ], self_tname |> immv)
    | CFLiteral l -> ([], l |> tliteral e.typ |> imml)
    | CFValue v -> ([], v |> tname e.typ |> immv)
    | CFTuple elements ->
        let inner (bindings, results) x =
          let new_binding, new_result = anf_of_expr x in
          (bindings @ new_binding, results @ [ new_result ])
        in
        let bindings, tuple_imms = List.fold_left inner ([], []) elements in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding = tup tuple_imms |> binding self_tname in
        (bindings @ [ self_binding ], self_tname |> immv)
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
    | _, _ -> []

  let rec anf_of_let_binding (l : cf_typ_let_binding) : anf_val_binding list =
    let inner bindings x =
      let new_binding = anf_of_let_binding x in
      bindings @ new_binding
    in
    let bindings = List.fold_left inner [] l.cf_body.cf_lets in
    let expr_bindings, expr_res = anf_of_expr l.cf_body.cf_expr in
    bindings @ expr_bindings @ lv_binds l.l_v expr_res

  let anf_of_fun_binding (l : cf_fun_let_binding) : anf_fun_binding =
    let inner bindings x =
      let new_binding = anf_of_let_binding x in
      bindings @ new_binding
    in
    let bindings = List.fold_left inner [] l.b.cf_lets in
    let expr_bindings, res = anf_of_expr l.b.cf_expr in
    let inner2 a =
      let arg_name = cnt_next () |> tname a.typ in
      let arg_imm = arg_name |> immv in
      let arg_decs = lv_binds a arg_imm in
      (arg_decs, arg_name)
    in
    let arg_decs, arg_names =
      List.map inner2 l.args |> fun x ->
      (List.concat (List.map fst x), List.map snd x)
    in
    let lets = arg_decs @ bindings @ expr_bindings in
    let body = { lets; res } in
    let env_vars = l.env_vars in
    let name = l.name in
    { name; args = arg_names; body; env_vars }

  let anf_of_cf (p : cf_typ_program) : anf_program =
    let inner xs = function
      | FunBinding fb -> xs @ [ AnfFun (anf_of_fun_binding fb) ]
      | ValBinding vb -> xs @ (anf_of_let_binding vb |> List.map aval)
    in
    List.fold_left inner [] p
end

module AnfOptimizations = struct
  open AnfTypeAst
  open Counter.Counter
  open Type_ast.TypeAst

  module DbName = struct
    type t = tname

    let compare (x : t) (y : t) = compare x.value y.value
  end

  module NameMoveMap = Map.Make (DbName)

  type avbl = anf_val_binding list
  type nmm = tname NameMoveMap.t

  let tname t name : tname = { typ = t; value = name }

  let try_rename (nmm : nmm) (n : tname) : tname =
    match NameMoveMap.find_opt n nmm with Some v -> v | None -> n

  let apply_moves_to_imm (nmm : nmm) (i : imm) : imm =
    match i with ImmVal x -> ImmVal (try_rename nmm x) | lit -> lit

  let rec apply_moves_to_expr (nmm : nmm) (e : anf_expr) : anf_expr =
    let rn_imm = apply_moves_to_imm nmm in
    match e with
    | AApply (x, args) ->
        let args = List.map rn_imm args in
        AApply (rn_imm x, args)
    | ATuple xs -> ATuple (List.map rn_imm xs)
    | Aite (i, t, e) ->
        let i = rn_imm i in
        let tlets, nmm = apply_moves_to_vals nmm t.lets in
        let tres = apply_moves_to_imm nmm t.res in
        let t = { lets = tlets; res = tres } in
        let elets, nmm = apply_moves_to_vals nmm e.lets in
        let eres = apply_moves_to_imm nmm e.res in
        let e = { lets = elets; res = eres } in
        Aite (i, t, e)
    | AImm i -> AImm (rn_imm i)
    | ATupleAccess (t, i) -> ATupleAccess (rn_imm t, i)
    | AClosure (cl, env) -> AClosure (try_rename nmm cl, List.map rn_imm env)

  and apply_moves_to_val (nmm : nmm) (b : anf_val_binding) =
    let nmm =
      match b.e with
      | AImm (ImmVal x) -> NameMoveMap.add b.name (try_rename nmm x) nmm
      | _ -> nmm
    in
    let e = apply_moves_to_expr nmm b.e in

    let b = { b with e } in
    let b = match b.e with AImm (ImmVal _) -> None | _ -> Some b in
    (b, nmm)

  and apply_moves_to_vals (nmm : nmm) (vals : avbl) : avbl * nmm =
    let deopt_lst = List.filter_map (fun x -> x) in
    let inner (binds, nmm) bind =
      let bind, nmm = apply_moves_to_val nmm bind in
      (bind :: binds, nmm)
    in
    let res, nmm =
      List.fold_left inner ([], nmm) vals |> fun (bs, nm) -> (List.rev bs, nm)
    in
    (deopt_lst res, nmm)

  let apply_moves_to_fun (nmm : nmm) (fn : anf_fun_binding) =
    let env_vars = List.map (try_rename nmm) fn.env_vars in
    let lets, nmm = apply_moves_to_vals nmm fn.body.lets in
    let res = apply_moves_to_imm nmm fn.body.res in
    let body = { lets; res } in
    ({ fn with body; env_vars }, nmm)

  let optimize_moves (p : anf_program) =
    let deopt_val x = match x with None -> [] | Some x -> [ AnfVal x ] in
    let inner (bindings, nmm) = function
      | AnfVal b ->
          let b, nmm = apply_moves_to_val nmm b in
          (bindings @ deopt_val b, nmm)
      | AnfFun fn ->
          let fn, nmm = apply_moves_to_fun nmm fn in
          (bindings @ [ AnfFun fn ], nmm)
    in
    let nmm = NameMoveMap.empty in
    List.fold_left inner ([], nmm) p |> fst

  (* Closure inlining *)
  (* if outer is returning allocated closure, we can inline inner *)

  let rec inline_closures all_fns lvl (fn : anf_fun_binding) =
    let find_fn name = List.find (fun x -> name.value = x.name.value) all_fns in
    let outer = fn in
    let last_op = List.rev outer.body.lets |> List.hd in
    match last_op.e with
    | AClosure (inner, args) when lvl > 0 ->
        let inner = find_fn inner |> inline_closures all_fns (lvl - 1) in
        let make_a_move left right = { name = left; e = AImm right } in
        let lets =
          outer.body.lets
          @ List.map2 make_a_move inner.env_vars args
          @ inner.body.lets
        in
        let res = inner.body.res in
        {
          name = cnt_next () |> tname outer.name.typ;
          args = outer.args @ inner.args;
          body = { lets; res };
          env_vars = outer.env_vars;
        }
    | _ -> fn

  (* module ApplyMergeMap = Map.Make (DbName)
     type amm = (tname * imm list) ApplyMergeMap

     let apply_merging_to_vals all_fns amm =
       List.fold_left (fun xs, amm x ->
         match x.e with
         | AApply(ImmVal fn, args) ->
           match ApplyMergeMap.find_opt fn with
           | Some f ->
             let new_fun = inline_closures all_fns 1 in
             let amm = ApplyMergeMap.add x.name (f, args)
           | None ->
             let amm = ApplyMergeMap.add x.name (fn, args) in
         )
     (* Will remove some partial applications but will generate more functions and create unused vars *)
     let optimize_apply_merging prog =
       let filter_fn = function AnfFun f -> Some f | _ -> None in
       let all_fns = prog |> List.filter_map filter_fn in*)
end
