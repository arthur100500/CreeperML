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
    | AITE of
        anf_body * anf_body * anf_body (* It's not imm for lazy evaluation *)
    | AImm of imm
    | ATupleAccess of imm * int (* Get rid of lvalues *)

  and anf_body = { lets : anf_val_binding list; res : imm }
  and anf_val_binding = { name : tname; e : anf_expr }

  type anf_fun_binding = { name : tname; arg : tname; body : anf_body }
  type anf_binding = AnfVal of anf_val_binding | AnfFun of anf_fun_binding
  type anf_program = anf_binding list

  (* Cool print *)
  let print_imm st = function
    | ImmVal x -> Format.sprintf "v(%d%s)" x.value (st x.typ)
    | ImmLit x -> Format.sprintf "l(%s%s)" (show_literal x.value) (st x.typ)

  let rec print_anf_expr st intd = function
    | AApply (x, y) -> Format.sprintf "%s %s" (print_imm st x) (print_imm st y)
    | ATuple xs ->
        Format.sprintf "(%s)"
        @@ List.fold_left (fun xs x -> xs ^ "," ^ print_imm st x) "" xs
    | AITE (i, t, e) ->
        let print_body b =
          let lets =
            List.fold_left
              (fun xs x ->
                xs ^ "\n" ^ print_anf_dec st (intd ^ "  ") (AnfVal x))
              "" b.lets
          in
          let expr = Format.sprintf "  %s%s" intd (print_imm st b.res) in
          Format.sprintf "%s\n%s" lets expr
        in
        Format.sprintf "if%s\n%sthen%s\n%selse%s" (print_body i)
      intd (print_body t) intd (print_body e)
    | ATupleAccess (t, e) -> Format.sprintf "%s[%d]" (print_imm st t) e
    | AImm i -> print_imm st i

  and print_anf_dec st intd = function
    | AnfVal x ->
        Format.sprintf "%slet (%d%s) = %s" intd x.name.value (st x.name.typ)
          (print_anf_expr st intd x.e)
    | AnfFun x ->
        let name, name_type = (x.name.value, st x.name.typ) in
        let arg, arg_type = (x.arg.value, st x.arg.typ) in
        let lets =
          List.fold_left
            (fun xs x -> xs ^ "\n" ^ print_anf_dec st (intd ^ "  ") (AnfVal x))
            "" x.body.lets
        in
        Format.sprintf "let_f (%d%s) (%d%s) = %s%s" name name_type arg arg_type
          lets (print_imm st x.body.res)

  let do_show_type t = ": " ^ show_ty t
  let dont_show_type _ = ""

  let show_anf_program =
    List.fold_left
      (fun xs x -> xs ^ "\n" ^ print_anf_dec dont_show_type "" x)
      ""
end

module AnfConvert = struct
  open AnfTypeAst
  open Type_ast.TypeAst
  open Closureconvert.ClosureAst
  open Counter.Counter

  (* constructors *)
  let app l r = AApply (l, r)
  let tup l = ATuple l
  let aite i t e = AITE (i, t, e)
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
        let if_bindings, if_imm = anf_of_expr ite.cond in
        let then_bindings, then_imm = anf_of_expr ite.t_body in
        let else_bindings, else_imm = anf_of_expr ite.f_body in
        let if_body = body if_bindings if_imm in
        let then_body = body then_bindings then_imm in
        let else_body = body else_bindings else_imm in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding =
          aite if_body then_body else_body |> binding self_tname
        in
        let all_bindings =
          if_bindings @ then_bindings @ else_bindings @ [ self_binding ]
        in
        (all_bindings, self_tname |> immv)
    | CFLiteral l -> ([], l |> tliteral e.typ |> imml)
    | CFValue v -> ([], v |> tname e.typ |> immv)
    | CFTuple elements ->
        let bindings, tuple_imms =
          List.fold_left
            (fun (bindings, results) x ->
              let new_binding, new_result = anf_of_expr x in
              (bindings @ new_binding, results @ [ new_result ]))
            ([], []) elements
        in
        let self_tname = cnt_next () |> tname e.typ in
        let self_binding = tup tuple_imms |> binding self_tname in
        (bindings @ [ self_binding ], self_tname |> immv)

  let rec lv_binds (lv : tlvalue) (er : imm) : anf_val_binding list =
    match (lv.value, lv.typ) with
    | DLvValue name, _ -> [ binding (tname lv.typ name) (imm er) ]
    | DLvTuple lvalues, TyTuple typs ->
        let zipped =
          List.map2 (fun x y -> (x, y)) lvalues typs
        in
        let decs =
          List.mapi
            (fun index (elem, etyp) ->
              let t_name = cnt_next () |> tname etyp in
              let access = ATupleAccess (er, index) |> binding t_name in
              let telem = tlvalue etyp elem in
              access :: lv_binds telem (t_name |> immv))
            zipped
        in
        List.concat decs
    | _, _ -> []

  let rec anf_of_let_binding (l : cf_typ_let_binding) :
      anf_val_binding list =
    let bindings =
      List.fold_left
        (fun bindings x ->
          let new_binding = anf_of_let_binding x in
          bindings @ new_binding)
        [] l.cf_body.cf_lets
    in
    let expr_bindings, expr_res = anf_of_expr l.cf_body.cf_expr in
    bindings @ expr_bindings @ lv_binds l.l_v expr_res

  let anf_of_fun_binding (l : cf_fun_let_binding) : anf_fun_binding
      =
    let bindings =
      List.fold_left
        (fun bindings x ->
          let new_binding = anf_of_let_binding x in
          bindings @ new_binding)
        [] l.b.cf_lets
    in
    let expr_bindings, res = anf_of_expr l.b.cf_expr in
    let arg_name = cnt_next () |> tname l.args.typ in
    let arg_imm = arg_name |> immv in
    let arg_decs = lv_binds l.args arg_imm in
    let lets = arg_decs @ bindings @ expr_bindings in
    let body = { lets; res } in
    { name = l.name; arg = arg_name; body }

  let anf_of_program (p : cf_typ_program) : anf_program =
    List.fold_left
      (fun xs x ->
        match x with
        | FunBinding fb -> xs @ [ AnfFun (anf_of_fun_binding fb) ]
        | ValBinding vb ->
            xs @ (anf_of_let_binding vb |> List.map (fun x -> AnfVal x)))
      [] p
end
