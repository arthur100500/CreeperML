module InferType = struct
  open Parser_ast.ParserAst

  type lvl = int

  and ground_typ = TInt | TString | TBool | TUnit
  [@@deriving show { with_path = false }]

  type t =
    | TArrow of t * t
    | TTuple of t list
    | TGround of ground_typ
    | TVar of tv ref

  and tv = Unbound of name * lvl | Link of t
  and 'a typed = { value : 'a; typ : t } [@@deriving show { with_path = false }]

  type 'a lvls = { value : 'a; mutable old_lvl : lvl; mutable new_lvl : lvl }
  and typ = t lvls [@@deriving show { with_path = false }]
end

module InferTypeUtils = struct
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
  let typed_value { value = v; typ = _ } = v
  let with_typ t v = { value = v; typ = t }
  let typ { value = _; typ = t } = t
  let lvl_value { value = v; old_lvl = _; new_lvl = _ } = v
  let with_lvls old_l new_l v = { value = v; old_lvl = old_l; new_lvl = new_l }
end

module TypeAst = struct
  open InferType
  open Parser_ast.ParserAst

  type typ_name = name typed
  and typ_literal = literal typed
  and typ_lvalue = lvalue typed [@@deriving show { with_path = false }]

  type typ_let_binding = {
    rec_f : rec_flag;
    l_v : typ_lvalue;
    body : typ_let_body;
  }

  and typ_let_body = { lets : typ_let_binding list; expr : typ_expr }

  and t_expr =
    | TApply of typ_expr * typ_expr
    | TLiteral of typ_literal
    | TValue of typ_name
    | TFun of { lvalue : typ_lvalue; body : typ_let_body }
    | TTuple of typ_expr list
    | TIfElse of { cond : typ_expr; t_body : typ_expr; f_body : typ_expr }

  and typ_expr = t_expr typed [@@deriving show { with_path = false }]

  type typ_program = typ_let_binding list
  [@@deriving show { with_path = false }]
end

module TypeAstUtils = struct
  open TypeAst

  let typ_let_binding rec_f l_v b = { rec_f; l_v; body = b }
  let typ_let_body ls e = { lets = ls; expr = e }
  let t_apply e1 e2 = TApply (e1, e2)
  let t_literal l = TLiteral l
  let t_value n = TValue n
  let t_fun l_v b = TFun { lvalue = l_v; body = b }
  let t_typle es = TTuple es
  let t_if_else c t f = TIfElse { cond = c; t_body = t; f_body = f }
end
