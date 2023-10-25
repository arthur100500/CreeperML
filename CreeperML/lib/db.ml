module DbTypeAst = struct
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Position.Position
  open Counter.Counter

  type ilvalue = DLvAny | DLvUnit | DLvValue of int | DLvTuple of ilvalue list
  [@@deriving show { with_path = false }]

  type db_lvalue = (ilvalue, ty) typed [@@deriving show { with_path = false }]

  type db_let_binding = {
    rec_f : rec_flag;
    l_v : db_lvalue;
    body : db_let_body;
  }

  and db_let_body = { lets : db_let_binding list; expr : db_expr }

  and d_expr =
    | DApply of db_expr * db_expr
    | DLiteral of literal
    | DValue of int
    | DFun of db_fun_body
    | DTuple of db_expr list
    | DIfElse of tif_else

  and db_fun_body = { lvalue : db_lvalue; b : db_let_body }
  and tif_else = { cond : db_expr; t_body : db_expr; f_body : db_expr }
  and db_expr = (d_expr, ty) typed [@@deriving show { with_path = false }]

  type db_program = db_let_binding list [@@deriving show { with_path = false }]

  (* END AST, BEGIN TRANSLATE*)

  module NameMap = Map.Make (String)

  type nm = int NameMap.t
  type 'a res = 'a * nm

  (* cons *)
  let typed typ value : ('a, 'b) typed = { typ; value }

  let rec names_of_lvalue (l : lvalue) =
    match l with
    | LvAny | LvUnit -> [ "any" ]
    | LvValue v -> [ v ]
    | LvTuple vs ->
        List.map (fun x -> value x |> names_of_lvalue) vs
        |> List.fold_left ( @ ) []

  let rec db_lv (nm : nm) (l : lvalue) =
    match l with
    | LvAny -> DLvAny
    | LvUnit -> DLvUnit
    | LvValue v -> DLvValue (NameMap.find v nm)
    | LvTuple vs ->
        List.map (fun x -> value x |> db_lv nm) vs |> fun x -> DLvTuple x

  let rec db_expr (nm : nm) (e : ty typ_expr) : db_expr =
    let _dbg_nm_rendered =
      NameMap.fold (fun k x xs -> Format.sprintf "%s %s-%d" xs k x) nm ""
    in
    match e.value with
    | TApply (l, r) ->
        let lr = db_expr nm l in
        let rr = db_expr nm r in
        DApply (lr, rr) |> typed e.typ
    | TIfElse ite ->
        let ir = db_expr nm ite.cond in
        let tr = db_expr nm ite.t_body in
        let er = db_expr nm ite.f_body in
        let cond = ir in
        let t_body = tr in
        let f_body = er in
        DIfElse { cond; t_body; f_body } |> typed e.typ
    | TLiteral l -> DLiteral l |> typed e.typ
    | TValue v -> (
        match NameMap.find_opt v nm with
        | None -> failwith "Variable not found"
        | Some x -> DValue x |> typed e.typ)
    | TTuple vs -> List.map (db_expr nm) vs |> fun x -> DTuple x |> typed e.typ
    | TFun f ->
        let all_names = names_of_lvalue f.lvalue.value in
        let nm =
          List.fold_left
            (fun nm n -> NameMap.add n (cnt_next ()) nm)
            nm all_names
        in
        let _dbg_nm_rendered =
          NameMap.fold (fun k x xs -> Format.sprintf "%s %s-%d" xs k x) nm ""
        in
        let lets, nm_inners =
          List.fold_left
            (fun (xs, nm) x ->
              let inner_r, nm = db_let x nm in
              (xs @ [ inner_r ], nm))
            ([], nm) f.b.lets
        in
        let expr = db_expr nm_inners f.b.expr in
        let b = { lets; expr } in
        let lvalue = db_lv nm f.lvalue.value |> typed f.lvalue.typ in
        let f = { lvalue; b } in
        DFun f |> typed e.typ

  and db_let (l : ty typ_let_binding) nm : db_let_binding res =
    let all_names = names_of_lvalue l.l_v.value in
    let nm =
      match l.rec_f with
      | Rec ->
          List.fold_left
            (fun nm n -> NameMap.add n (cnt_next ()) nm)
            nm all_names
      | NoRec -> nm
    in
        let _dbg_nm_rendered =
      NameMap.fold (fun k x xs -> Format.sprintf "%s %s-%d" xs k x) nm ""
    in
    let _dbg_l_rendered = show_typ_let_binding (fun _ _ -> ()) l in
    let lets, nm_inners =
      List.fold_left
        (fun (xs, nm) x ->
          let inner_r, nm = db_let x nm in
          (xs @ [ inner_r ], nm))
        ([], nm) l.body.lets
    in
    let expr = l.body.expr |> db_expr nm_inners in
    let body = { lets; expr } in
    let nm =
      match l.rec_f with
      | Rec -> nm
      | NoRec ->
          List.fold_left
            (fun nm n -> NameMap.add n (cnt_next ()) nm)
            nm all_names
    in
    let l_v = db_lv nm l.l_v.value |> typed l.l_v.typ in
    ({ rec_f = l.rec_f; body; l_v }, nm)

  (* Move declarations of let inside of fun *)
  let rec move_lets (l : ty typ_let_binding) : ty typ_let_binding =
    match l.body.expr.value with
    | TFun f ->
        let inners = l.body.lets @ f.b.lets in
        let inners = List.map move_lets inners in
        let new_fun = { f with b = { f.b with lets = inners } } in
        let expr = TFun new_fun |> typed l.body.expr.typ in
        { l with body = { lets = []; expr } }
    | _ -> l

  let db_program_of_typed_program (nm : nm) (p : ty typ_program) : db_program =
    let p = List.map move_lets p in
    let res, _ =
      List.fold_left
        (fun (xs, nm) x ->
          let res, nm = db_let x nm in
          (xs @ [ res ], nm))
        ([], nm) p
    in
    res
end
