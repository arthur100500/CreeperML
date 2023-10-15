(*module AnfTypeAst = struct
    open Type_ast.TypeAst
    open Parser_ast.ParserAst
    open Position.Position

    (* generator of de breujn names (super counter) *)
    let name_count = ref 0

    let gen_name () =
      name_count := !name_count + 1;
      !name_count

    (* mapping of existing names to de breujn variants *)
    module NameMap = Map.Make (String)

    (* Typed AST in ANF and De breujn indices *)
    type ilvalue = ILvUnit | ILvValue of int | ILvTuple of ilvalue list | ILvAny
    [@@deriving show { with_path = false }]

    type db_name = ilvalue typed [@@deriving show { with_path = false }]

    type imm = ImmVal of int typed | ImmLit of literal typed
    [@@deriving show { with_path = false }]

    (* Exprs with simple apply, tuples, vals and literals, ite constructions *)
    type anf_expr =
      | AApply of imm * imm
      | ATuple of imm list
      | AITE of imm * imm * imm
    [@@deriving show { with_path = false }]

    (* Let wit arguments and expr (name, args, res)*)
    type anf_let = { name : int typed; expr : anf_expr }
    [@@deriving show { with_path = false }]

    type fun_let = {
      is_rec : bool;
      name : int typed;
      arg : db_name;
      lets : let_binding list;
      res : imm;
    }
    [@@deriving show { with_path = false }]

    and let_binding = Function of fun_let | Binding of anf_let
    [@@deriving show { with_path = false }]

    (* Easy constructors *)
    let tname i t : db_name = { value = i; typ = t }
    let iname i t : int typed = { value = i; typ = t }
    let tlit l t : literal typed = { value = l; typ = t }
    let immlit x = ImmLit x
    let immval x = ImmVal x
    let ilvval x = ILvValue x
    let dec name expr = { name; expr }
    let fun_dec is_rec name arg lets res = { is_rec; name; arg; lets; res }
    let bind x = Binding x
    let fun_bind x = Function x

    let rec gen_db_name (lval : typ_lvalue) vm =
      match (lval.value, lval.typ) with
      | LvValue old_name, _ ->
          let idname = gen_name () in
          let name = tname (idname |> ilvval) lval.typ in
          let vm = NameMap.add old_name idname vm in
          (name, vm)
      | LvTuple old_names, TyTuple typs ->
          let helper (acc, vm) x =
            let genned, vm = gen_db_name { value = snd x; typ = fst x } vm in
            (acc @ [ genned.value ], vm)
          in
          let res, vm =
            List.map value old_names
            |> List.map2 (fun x y -> (x, y)) typs
            |> List.fold_left helper ([], vm)
          in
          (tname (ILvTuple res) lval.typ, vm)
      (* Very unsure of type, ask Matvey *)
      | LvUnit, _ -> (tname ILvUnit (TyGround TUnit), vm)
      | _, _ -> (tname ILvAny (TyGround TUnit), vm)

    let rec process_lets lst vm acc =
      match lst with
      | h :: t ->
          let lets, res, vm = anf_let h vm in
          let iter_res = (lets, res) in
          process_lets t vm (acc @ [ iter_res ])
      | [] -> (acc, vm)

    (* ANF of whole program *)
    and anf_of_program (program : typ_program) =
      let vm = NameMap.empty in
      let res, _ = process_lets program vm [] in
      res |> List.concat_map fst

    (* ANF form of let *)
    and anf_let (l : typ_let_binding) vm =
      let lets = l.body.lets in
      let expr = l.body.expr in
      let results, vm = process_lets lets vm [] in
      let _, bvm = gen_db_name l.l_v vm in
      let self_dec, self_name, _ = anf_expr expr bvm in
      let inner_decs = List.concat_map fst results in
      (self_dec @ inner_decs, self_name, bvm)

    (* let of fn *)
    and anf_fun f is_rec vm =
      let arg = f.lvalue in
      let body = f.b in
      let bvm = vm in
      let arg_name, vm = gen_db_name arg vm in
      let self_name = iname (gen_name ()) f.b.expr.typ in
      let lets = body.lets in
      let expr = body.expr in
      let results, vm = process_lets lets vm [] in
      let self_dec, imm_result, _ = anf_expr expr vm in
      let inner_decs = List.concat_map fst results in
      let self_val = self_name |> immval in
      let all_decs = inner_decs @ self_dec in
      (fun_dec is_rec self_name arg_name all_decs imm_result, self_val, bvm)

    (* ANF expr and alet decs from expr *)
    and anf_expr (e : typ_expr) vm =
      let rec process_exprs lst vm acc =
        match lst with
        | h :: t ->
            let lets, res, vm = anf_expr h vm in
            let iter_res = (lets, res) in
            process_exprs t vm (acc @ [ iter_res ])
        | [] -> (acc, vm)
      in
      match e.value with
      | TApply (left, right) ->
          let left_lets, left_res, vm = anf_expr left vm in
          let right_lets, right_res, vm = anf_expr right vm in
          let name = iname (gen_name ()) e.typ in
          (* APPLY MAY CHANGE LATER, FIX IF BROKEN *)
          let dec = dec name @@ AApply (right_res, left_res) |> bind in
          (left_lets @ right_lets @ [ dec ], name |> immval, vm)
      | TLiteral literal -> ([], tlit literal e.typ |> immlit, vm)
      | TValue name -> (
          match NameMap.find_opt name vm with
          | None ->
              let idname = gen_name () in
              let dn = iname idname e.typ in
              let vm = NameMap.add name idname vm in
              ([], dn |> immval, vm)
          | Some i ->
              let dn = iname i e.typ in
              ([], dn |> immval, vm))
      | TTuple exprs ->
          let results, vm = process_exprs exprs vm [] in
          let res = ATuple (List.map snd results) in
          let name = iname (gen_name ()) e.typ in
          let dec = dec name res |> bind in
          let decs = List.concat_map fst results in
          (decs @ [ dec ], name |> immval, vm)
      | TFun fn ->
          let dec, res, vm = anf_fun fn false vm in
          ([ dec |> fun_bind ], res, vm)
      | TIfElse ite ->
          let c_decs, c_res, vm = anf_expr ite.cond vm in
          let t_decs, t_res, vm = anf_expr ite.t_body vm in
          let d_decs, f_res, vm = anf_expr ite.f_body vm in
          let name = iname (gen_name ()) e.typ in
          let res = AITE (c_res, t_res, f_res) in
          let dec = dec name res |> bind in
          (c_decs @ t_decs @ d_decs @ [ dec ], name |> immval, vm)

    (* Cool print *)
    let print_imm st = function
      | ImmVal x -> Format.sprintf "v(%d%s)" x.value (st x.typ)
      | ImmLit x -> Format.sprintf "l(%s%s)" (show_literal x.value) (st x.typ)

    let print_anf_expr st = function
      | AApply (x, y) -> Format.sprintf "%s %s" (print_imm st x) (print_imm st y)
      | ATuple xs ->
          Format.sprintf "(%s)"
          @@ List.fold_left (fun xs x -> xs ^ "," ^ print_imm st x) "" xs
      | AITE (i, t, e) ->
          Format.sprintf "if %s then %s else %s" (print_imm st i) (print_imm st t)
            (print_imm st e)

    let rec print_anf_dec st intd = function
      | Binding x ->
          Format.sprintf "%slet (%d%s) = %s" intd x.name.value (st x.name.typ)
            (print_anf_expr st x.expr)
      | Function x ->
          let name, name_type = (x.name.value, st x.name.typ) in
          let arg, arg_type = (show_ilvalue x.arg.value, st x.arg.typ) in
          let lets =
            List.fold_left
              (fun xs x -> xs ^ "\n" ^ print_anf_dec st (intd ^ "  ") x)
              "" x.lets
          in
          Format.sprintf "let_f (%d%s) (%s%s) = %s%s" name name_type arg arg_type
            lets (print_imm st x.res)

    let do_show_type t = ": " ^ show_ty t
    let dont_show_type _ = ""

    let show_anf_program =
      List.fold_left (fun xs x -> xs ^ print_anf_dec do_show_type "" x) ""
  end
*)
