(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserAst = struct
  type name = string [@@deriving show { with_path = false }]
  type rec_flag = Rec | NoRec [@@deriving show { with_path = false }]

  type lvalue = LvAny | LvUnit | LvValue of name | LvTuple of lvalue list
  [@@deriving show { with_path = false }]

  type literal =
    | LInt of int
    | LFloat of float
    | LString of string
    | LBool of bool
    | LUnit
  [@@deriving show { with_path = false }]

  type let_binding = { rec_f : rec_flag; l_v : lvalue; body : let_body }
  and let_body = { lets : let_binding list; expr : expr }

  and expr =
    | EApply of expr * expr
    | ELiteral of literal
    | EValue of name
    | EFun of lvalue * let_body
    | ETuple of expr list
    | EIfElse of if_else_body
  [@@deriving show { with_path = false }]

  and if_else_body = { cond : expr; t_body : expr; f_body : expr }

  type program = let_binding list [@@deriving show { with_path = false }]
end

module ParserAstUtils = struct
  open ParserAst

  let rec_f = Rec
  let norec_f = NoRec
  let lv_any = LvAny
  let lv_unit = LvUnit
  let lv_value n = LvValue n
  let lv_tuple lvs = LvTuple lvs
  let l_int n = LInt n
  let l_float n = LFloat n
  let l_string s = LString s
  let l_bool f = LBool f
  let l_unit = LUnit
  let e_apply e1 e2 = EApply (e1, e2)
  let e_literal l = ELiteral l
  let e_value n = EValue n
  let e_fun l b = EFun (l, b)
  let e_tuple es = ETuple es
  let e_if_else c t f = EIfElse { cond = c; t_body = t; f_body = f }

  let let_binding ?(rec_flag = norec_f) lv body =
    { rec_f = rec_flag; l_v = lv; body }

  let let_body ls e = { lets = ls; expr = e }
end
