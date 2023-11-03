(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module DbTypeAst : sig
  open Type_ast.TypeAst
  open Parser_ast.ParserAst

  type ilvalue = DLvAny | DLvUnit | DLvValue of int | DLvTuple of ilvalue list
  type db_lvalue = (ilvalue, ty) typed

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
  and db_expr = (d_expr, ty) typed

  type db_program = db_let_binding list

  module NameMap : sig
    type key = string
    type 'a t = 'a Map.Make(String).t

    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
  end

  val db_of_typed : int NameMap.t -> ty typ_program -> db_program
end
