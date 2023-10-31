(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ClosureAst : sig
  open Db.DbTypeAst
  open Parser_ast.ParserAst
  open Type_ast.TypeAst

  type cf_expr =
    | CFApply of cf_typ_expr * cf_typ_expr
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
    l_v : db_lvalue;
    cf_body : cf_typ_let_body;
  }

  and cf_typ_let_body = {
    cf_lets : cf_typ_let_binding list;
    cf_expr : cf_typ_expr;
  }

  type cf_fun_let_binding = {
    is_rec : rec_flag;
    name : (int, ty) typed;
    args : db_lvalue;
    b : cf_typ_let_body;
    env_vars : (int, ty) typed list;
  }

  type cf_binding =
    | FunBinding of cf_fun_let_binding
    | ValBinding of cf_typ_let_binding

  type cf_typ_program = cf_binding list
end

module ClosureConvert : sig
  open ClosureAst
  open Type_ast.TypeAst
  open Db.DbTypeAst

  val cf_of_db : (int, ty) typed list -> db_program -> cf_typ_program
end
