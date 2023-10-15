module ClosureAst = struct
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Position.Position

  type cf_typ_let_binding = {
    rec_f : rec_flag;
    l_v : typ_lvalue;
    cf_body : cf_typ_let_body;
  }

  and cf_typ_let_body = {
    cf_lets : cf_typ_let_binding list;
    cf_expr : cf_typ_expr;
  }

  and typ_fun_let_binding = {
    is_rec : rec_flag;
    name : name typed;
    args : typ_lvalue list;
    b : cf_typ_let_body;
    env_vars : name typed list;
  }

  and cf_expr =
    | CFApply of cf_typ_expr * cf_typ_expr
    | CFLiteral of literal
    | CFValue of name
    | CFTuple of cf_typ_expr list
    | CFIfElse of tif_else

  and tif_else = {
    cond : cf_typ_expr;
    t_body : cf_typ_expr;
    f_body : cf_typ_expr;
  }

  and cf_typ_expr = cf_expr typed [@@deriving show { with_path = false }]

  type cf_binding =
    | FunBinding of typ_fun_let_binding
    | ValBinding of cf_typ_let_binding
  [@@deriving show { with_path = false }]

  type cf_typ_program = cf_binding list [@@deriving show { with_path = false }]

  (* Cool print *)
  let print_env_vars st (c : name typed list) =
    List.fold_left
      (fun xs (x : name typed) ->
        Format.sprintf "[%s %s] %s" x.value (st x.typ) xs)
      "" c
    |> Format.sprintf "[%s]"

  let rec print_cf_expr st (e : cf_typ_expr) =
    match e.value with
    | CFApply (x, y) ->
        Format.sprintf "%s %s" (print_cf_expr st x) (print_cf_expr st y)
    | CFTuple xs ->
        Format.sprintf "(%s)"
        @@ List.fold_left (fun xs x -> xs ^ "," ^ print_cf_expr st x) "" xs
    | CFIfElse ite ->
        let i = ite.cond in
        let t = ite.t_body in
        let e = ite.f_body in
        Format.sprintf "if %s then %s else %s" (print_cf_expr st i)
          (print_cf_expr st t) (print_cf_expr st e)
    | CFValue v -> v
    | CFLiteral l -> show_literal l

  let rec print_lval = function
    | LvValue v -> v
    | LvAny -> "any"
    | LvUnit -> "()"
    | LvTuple xs ->
        Format.sprintf "(%s)"
        @@ List.fold_left (fun xs x -> xs ^ "," ^ print_lval (value x)) "" xs

  let rec print_cf_dec st intd = function
    | ValBinding x ->
        let lval = print_lval x.l_v.value in
        let lets =
          List.fold_left
            (fun xs x -> xs ^ "\n" ^ print_cf_dec st (intd ^ "  ") x)
            intd
            (List.map (fun x -> ValBinding x) x.cf_body.cf_lets)
        in
        Format.sprintf "%slet (%s%s) = %s\n%s  %s\n\n" intd lval (st x.l_v.typ)
          lets intd
          (print_cf_expr st x.cf_body.cf_expr)
    | FunBinding x ->
        let lval = x.name.value in
        let lets =
          List.fold_left
            (fun xs x -> xs ^ "\n" ^ print_cf_dec st (intd ^ "  ") x)
            intd
            (List.map (fun x -> ValBinding x) x.b.cf_lets)
        in
        let args =
          List.fold_left
            (fun xs (x : typ_lvalue) ->
              Format.sprintf "%s (%s%s)" xs (print_lval x.value) (st x.typ))
            "" x.args
        in
        let env_vals = print_env_vars st x.env_vars in
        Format.sprintf "%sletf %s %s%s %s= %s\n%s  %s\n\n" intd lval args
          (st x.name.typ) env_vals lets intd
          (print_cf_expr st x.b.cf_expr)

  let do_show_type t = ": " ^ show_ty t
  let dont_show_type _ = ""

  let show_cf_program =
    List.fold_left (fun xs x -> xs ^ print_cf_dec do_show_type "" x) ""
end

module ClosureConvert = struct
  open ClosureAst
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Position.Position

  module TypedName = struct
    type t = name typed

    let compare (n1 : t) (n2 : t) = compare n1.value n2.value
  end

  module NameSet = Set.Make (TypedName)

  let name_count = ref 0

  let gen_fun_name () =
    name_count := !name_count + 1;
    Format.sprintf "f.%d" !name_count

  let typed t e : 'a typed = { value = e; typ = t }

  let rec typ_names_of_lvalue (lval : lvalue typed) =
    match (lval.value, lval.typ) with
    | LvTuple vs, TyTuple typs ->
        let e = NameSet.empty in
        let valtyps = List.map2 (fun x y -> (x, y)) vs typs in
        List.fold_left
          (fun xs x ->
            NameSet.union xs
            @@ typ_names_of_lvalue (typed (snd x) @@ value (fst x)))
          e valtyps
    | LvValue v, ty -> typed ty v |> NameSet.singleton
    | LvUnit, ty -> typed ty "()" |> NameSet.singleton
    | LvAny, ty -> typed ty "_" |> NameSet.singleton
    | _ -> failwith "Incorrectly typed ast"

  let rec collect_unbound_variables (f : tfun_body) global_bindings =
    let rec collect_variables_in_expr (e : typ_expr) =
      match e.value with
      | TApply (left, right) ->
          let left = collect_variables_in_expr left in
          let right = collect_variables_in_expr right in
          NameSet.union left right
      | TLiteral _ -> NameSet.empty
      | TValue name -> NameSet.singleton @@ typed e.typ name
      | TTuple exprs ->
          List.fold_left NameSet.union NameSet.empty
            (List.map collect_variables_in_expr exprs)
      | TIfElse ite ->
          let i = collect_variables_in_expr ite.cond in
          let t = collect_variables_in_expr ite.t_body in
          let e = collect_variables_in_expr ite.f_body in
          NameSet.union i t |> NameSet.union e
      | TFun f -> collect_unbound_variables f global_bindings
    in
    let rec collect_variables_in_let (l : typ_let_binding) =
      let e = NameSet.empty in
      let inner =
        List.fold_left
          (fun xs x -> NameSet.union xs @@ collect_variables_in_let x)
          e l.body.lets
      in
      let self = typ_names_of_lvalue l.l_v in
      NameSet.union inner self
    in
    let collect_variables_in_body (b : typ_let_body) =
      let e = NameSet.empty in
      List.fold_left
        (fun xs x -> NameSet.union xs @@ collect_variables_in_let x)
        e b.lets
      |> NameSet.union @@ collect_variables_in_expr b.expr
    in
    let known = NameSet.union global_bindings @@ typ_names_of_lvalue f.lvalue in
    let unknown = collect_variables_in_body f.b in
    NameSet.diff unknown known

  let rec closure_free_expr globals r (e : typ_expr) =
    match e.value with
    | TApply (left, right) ->
        let left_decs, left = closure_free_expr globals r left in
        let right_decs, right = closure_free_expr globals r right in
        let result = CFApply (left, right) |> typed e.typ in
        (left_decs @ right_decs, result)
    | TLiteral literal -> ([], CFLiteral literal |> typed e.typ)
    | TValue name -> ([], CFValue name |> typed e.typ)
    | TTuple exprs ->
        let inner xs x =
          let x_decs, x_res = closure_free_expr globals r x in
          (fst xs @ x_decs, snd xs @ [ x_res ])
        in
        let cf_exprs = List.fold_left inner ([], []) exprs in
        (fst cf_exprs, CFTuple (snd cf_exprs) |> typed e.typ)
    | TIfElse ite ->
        let i_decs, i_expr = closure_free_expr globals r ite.cond in
        let t_decs, t_expr = closure_free_expr globals r ite.t_body in
        let e_decs, e_expr = closure_free_expr globals r ite.f_body in
        let res =
          CFIfElse { cond = i_expr; t_body = t_expr; f_body = e_expr }
        in
        let res_typed = res |> typed e.typ in
        (i_decs @ t_decs @ e_decs, res_typed)
    | TFun f ->
        let unknown_vars = collect_unbound_variables f globals in
        let inner = List.map (cf_let globals r) f.b.lets in
        let expr_closures, cf_expr = closure_free_expr globals r f.b.expr in
        let inner_cf_lets = List.map snd inner in
        let inner_closures = List.map fst inner in
        let cf_body = { cf_lets = inner_cf_lets; cf_expr } in
        let env = NameSet.to_seq unknown_vars |> List.of_seq in
        let genned = gen_fun_name () in
        let fun_let =
          {
            is_rec = r;
            name = typed e.typ genned;
            args = [ f.lvalue ];
            b = cf_body;
            env_vars = env;
          }
        in
        let f = CFValue genned |> typed e.typ in
        (List.concat inner_closures @ expr_closures @ [ fun_let ], f)

  and cf_let (globals : NameSet.t) is_rec (l : typ_let_binding) =
    let inner = List.map (cf_let globals is_rec) l.body.lets in
    let closures, cf_expr = closure_free_expr globals is_rec l.body.expr in
    let inner_cf_lets = List.map snd inner in
    let inner_closures = List.map fst inner in
    let cf_let_body : cf_typ_let_body = { cf_lets = inner_cf_lets; cf_expr } in
    let cf_let_binding =
      { rec_f = l.rec_f; l_v = l.l_v; cf_body = cf_let_body }
    in
    (List.concat inner_closures @ closures, cf_let_binding)

  and cf_program (prog : typ_program) (globals : name typed list) :
      cf_typ_program =
    let rec inner g (p : typ_program) acc =
      match p with
      | h :: t ->
          let closures, binding = cf_let g h.rec_f h in
          let closures = List.map (fun x -> FunBinding x) closures in
          let binding = ValBinding binding in
          let u = NameSet.union in
          let g = u (typ_names_of_lvalue (h.l_v.value |> typed h.l_v.typ)) g in
          (closures @ [ binding ]) @ acc |> inner g t
      | [] -> acc
    in
    let set = NameSet.of_list globals in
    inner set prog []
end
