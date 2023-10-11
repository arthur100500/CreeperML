(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module InferType = struct
  open Parser_ast.ParserAst

  type lvl = int

  and ground_typ = TInt | TString | TBool | TUnit | TFloat
  [@@deriving show { with_path = false }]

  type ty =
    | TArrow of typ * typ
    | TTuple of typ list
    | TGround of ground_typ
    | TVar of tv ref

  and tv = Unbound of name * lvl | Link of typ
  and 'a lvls = { value : 'a; mutable old_lvl : lvl; mutable new_lvl : lvl }
  and typ = ty lvls [@@deriving show { with_path = false }]

  type env = (name * typ) list
end

module InferTypeUtils = struct
  open Parser_ast.ParserAst
  open InferType

  let t_int = TInt
  let t_string = TString
  let t_bool = TBool
  let t_unit = TUnit
  let t_arrow t1 t2 = TArrow (t1, t2)
  let t_tuple ts = TTuple ts
  let t_ground t = TGround t
  let t_var t = TVar t
  let tv_unbound n l = Unbound (n, l)
  let tv_link t = Link t
  let lvl_value { value = v; old_lvl = _; new_lvl = _ } = v

  let is_unit typ =
    match lvl_value typ with TGround TUnit -> true | _ -> false

  let with_lvls old_l new_l v = { value = v; old_lvl = old_l; new_lvl = new_l }
  let assoc = List.assoc

  let convert_const l =
    let l = Position.Position.value l in
    (match l with
    | LInt _ -> TInt
    | LString _ -> TString
    | LBool _ -> TBool
    | LFloat _ -> TFloat
    | LUnit -> TUnit)
    |> t_ground

  let rec repr t =
    match lvl_value t with
    | TVar ({ contents = Link t } as tvar) ->
        let t = repr t in
        tvar := Link t;
        t
    | _ -> t
end

module TypeAst = struct
  open InferType
  open Parser_ast.ParserAst

  type ty =
    | TyArrow of ty * ty
    | TyTuple of ty list
    | TyGround of ground_typ
    | TyVar of name

  and 'a typed = { value : 'a; typ : ty }
  [@@deriving show { with_path = false }]

  type typ_lvalue = lvalue typed [@@deriving show { with_path = false }]

  type typ_let_binding = {
    rec_f : rec_flag;
    l_v : typ_lvalue;
    body : typ_let_body;
  }

  and typ_let_body = { lets : typ_let_binding list; expr : typ_expr }

  and t_expr =
    | TApply of typ_expr * typ_expr
    | TLiteral of literal
    | TValue of name
    | TFun of tfun_body
    | TTuple of typ_expr list
    | TIfElse of tif_else

  and tfun_body = { lvalue : typ_lvalue; b : typ_let_body }
  and tif_else = { cond : typ_expr; t_body : typ_expr; f_body : typ_expr }
  and typ_expr = t_expr typed [@@deriving show { with_path = false }]

  type typ_program = typ_let_binding list
  [@@deriving show { with_path = false }]
end

module TypeAstUtils = struct
  open TypeAst
  open InferTypeUtils

  let ty_arrow t1 t2 = TyArrow (t1, t2)
  let ty_tuple ts = TyTuple ts
  let ty_ground t = TyGround t
  let ty_var n = TyVar n
  let typed_value { value = v; typ = _ } = v
  let with_typ t v = { value = v; typ = t }
  let typ { value = _; typ = t } = t
  let typ_let_binding rec_f l_v b = { rec_f; l_v; body = b }
  let typ_let_body ls e = { lets = ls; expr = e }
  let t_apply e1 e2 = TApply (e1, e2)
  let t_literal l = TLiteral l
  let t_value n = TValue n
  let t_fun l_v b = TFun { lvalue = l_v; b }
  let t_tuple es = TTuple es
  let t_if_else c t f = TIfElse { cond = c; t_body = t; f_body = f }

  let rec remove_lvl typ =
    let open InferType in
    match lvl_value typ with
    | TVar { contents = Unbound (n, _) } -> ty_var n
    | TVar { contents = Link t } -> remove_lvl t
    | TArrow (t1, t2) -> remove_lvl t1 |> fun t1 -> remove_lvl t2 |> ty_arrow t1
    | TTuple ts -> List.map remove_lvl ts |> ty_tuple
    | TGround t -> ty_ground t
end
