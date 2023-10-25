module AnfTypeAst = struct
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Db.DbTypeAst

  type tlvalue = db_lvalue
  type tliteral = (literal, ty) typed
  type tname = (int, ty) typed
  type imm = ImmVal of tname | ImmLit of tliteral

  type anf_expr =
    | AApply of imm * imm
    | ATuple of imm list
    | Aite of anf_body * anf_body * anf_body
    | AImm of imm
    | ATupleAccess of imm * int
    | AClosure of imm * imm list

  and anf_body = { lets : anf_val_binding list; res : imm }
  and anf_val_binding = { name : tname; e : anf_expr }

  type anf_fun_binding = {
    name : tname;
    arg : tname;
    body : anf_body;
    env_vars : tname list;
  }

  type anf_binding = AnfVal of anf_val_binding | AnfFun of anf_fun_binding
  type anf_program = anf_binding list

  (* Cool print *)
  let rec join sep lst =
    match lst with
    | h :: [] -> h
    | h :: t -> Format.sprintf "%s%s%s" h sep (join sep t)
    | [] -> ""

  let print_imm st = function
    | ImmVal x -> Format.sprintf "v(%d%s)" x.value (st x.typ)
    | ImmLit x -> Format.sprintf "l(%s%s)" (show_literal x.value) (st x.typ)

  let rec print_body st intd b =
    let inner xs x = xs ^ "\n" ^ print_anf_dec st (intd ^ "  ") (AnfVal x) in
    let lets = List.fold_left inner "" b.lets in
    let expr = Format.sprintf "  %s%s" intd (print_imm st b.res) in
    Format.sprintf "%s\n%s" lets expr

  and print_anf_expr st intd = function
    | AApply (x, y) -> Format.sprintf "%s %s" (print_imm st x) (print_imm st y)
    | ATuple xs ->
        Format.sprintf "(%s)"
        @@ List.fold_left (fun xs x -> xs ^ "," ^ print_imm st x) "" xs
    | Aite (i, t, e) ->
        let i_b = print_body st intd i in
        let t_b = print_body st intd t in
        let f_b = print_body st intd e in
        Format.sprintf "if%s\n%sthen%s\n%selse%s" i_b intd t_b intd f_b
    | ATupleAccess (t, e) -> Format.sprintf "%s[%d]" (print_imm st t) e
    | AImm i -> print_imm st i
    | AClosure (i, env) ->
        let env = join ", " @@ List.map (print_imm st) env in
        Format.sprintf "clsr[%s][%s]" (print_imm st i) env

  and print_anf_dec st intd = function
    | AnfVal x ->
        Format.sprintf "%slet (%d%s) = %s" intd x.name.value (st x.name.typ)
          (print_anf_expr st intd x.e)
    | AnfFun x ->
        let print_env_var x = x.value |> Format.sprintf "%d" in
        let intd = intd ^ "  " in
        let inner xs x = xs ^ "\n" ^ print_anf_dec st intd (AnfVal x) in
        let name, name_type = (x.name.value, st x.name.typ) in
        let arg, arg_type = (x.arg.value, st x.arg.typ) in
        let lets = List.fold_left inner "" x.body.lets in
        let env_vars = join ", " @@ List.map print_env_var x.env_vars in
        Format.sprintf "let_f (%d%s) (%d%s) [%s] = %s\n%s%s" name name_type arg
          arg_type env_vars lets intd (print_imm st x.body.res)

  let show_anf_program print_type =
    let do_show_type t = ": " ^ show_ty t in
    let dont_show_type _ = "" in
    let st = if print_type then do_show_type else dont_show_type in
    let inner xs x = Format.sprintf "%s\n%s\n" xs (print_anf_dec st "" x) in
    List.fold_left inner ""
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
    | CFApply (left, right) ->
        let left_bindings, left_imm = anf_of_expr left in
        let right_bindings, right_imm = anf_of_expr right in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding = app left_imm right_imm |> binding self_tname in
        (left_bindings @ right_bindings @ [ self_binding ], self_tname |> immv)
    | CFIfElse ite ->
        let i_bindings, if_imm = anf_of_expr ite.cond in
        let t_bindings, then_imm = anf_of_expr ite.t_body in
        let e_bindings, else_imm = anf_of_expr ite.f_body in
        let i_body = body i_bindings if_imm in
        let t_body = body t_bindings then_imm in
        let e_body = body e_bindings else_imm in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding = aite i_body t_body e_body |> binding self_tname in
        let all_bindings =
          i_bindings @ t_bindings @ e_bindings @ [ self_binding ]
        in
        (all_bindings, self_tname |> immv)
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
          AClosure (c |> tname e.typ |> immv, env) |> binding self_tname
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
    let arg_name = cnt_next () |> tname l.args.typ in
    let arg_imm = arg_name |> immv in
    let arg_decs = lv_binds l.args arg_imm in
    let lets = arg_decs @ bindings @ expr_bindings in
    let body = { lets; res } in
    let env_vars = l.env_vars in
    { name = l.name; arg = arg_name; body; env_vars }

  let anf_of_program (p : cf_typ_program) : anf_program =
    let inner xs x =
      match x with
      | FunBinding fb -> xs @ [ AnfFun (anf_of_fun_binding fb) ]
      | ValBinding vb -> xs @ (anf_of_let_binding vb |> List.map aval)
    in
    List.fold_left inner [] p
end

(*
module AnfOptimizations = struct
  open AnfTypeAst

  module DbName = struct
    type t = tname

    let compare (x : t) (y : t) = compare x.value y.value
  end

  module NameMoveMap = Map.Make (DbName)

  type avbl = anf_val_binding list
  type nmm = tname NameMoveMap.t

  let try_rename (nmm : nmm) (n : tname) : tname =
    match NameMoveMap.find_opt n nmm with Some v -> v | None -> n

  let apply_moves_to_imm (nmm : nmm) (i : imm) : imm =
    match i with ImmVal x -> ImmVal (try_rename nmm x) | lit -> lit

  let rec apply_moves_to_expr (nmm : nmm) (e : anf_expr) : anf_expr =
    let rn_imm = apply_moves_to_imm nmm in
    match e with
    | AApply (x, y) -> AApply (rn_imm x, rn_imm y)
    | ATuple xs -> ATuple (List.map rn_imm xs)
    | Aite (i, t, e) ->
        let ilets, nmm = apply_moves_to_vals nmm i.lets in
        let ires = apply_moves_to_imm nmm i.res in
        let i = { lets = ilets; res = ires } in
        let tlets, nmm = apply_moves_to_vals nmm t.lets in
        let tres = apply_moves_to_imm nmm t.res in
        let t = { lets = tlets; res = tres } in
        let elets, nmm = apply_moves_to_vals nmm e.lets in
        let eres = apply_moves_to_imm nmm e.res in
        let e = { lets = elets; res = eres } in
        Aite (i, t, e)
    | AImm i -> AImm (rn_imm i)
    | ATupleAccess (t, i) -> ATupleAccess (rn_imm t, i)

  and apply_moves_to_val (nmm : nmm) (b : anf_val_binding) =
    let nmm =
      match b.e with
      | AImm (ImmVal x) -> NameMoveMap.add b.name x nmm
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
      (binds @ [ bind ], nmm)
    in
    let res, nmm = List.fold_left inner ([], nmm) vals in
    (deopt_lst res, nmm)

  let apply_moves_to_fun (nmm : nmm) (fn : anf_fun_binding) = 
    failwith "123"

  let reduce_moves anf_program =
    let nmm = NameMoveMap.empty in
    failwith "123"
end
*)
