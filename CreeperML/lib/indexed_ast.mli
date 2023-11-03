(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module IndexedTypeAst : sig
  open Type_ast.TypeAst
  open Parser_ast.ParserAst

  type ilvalue = DLvAny | DLvUnit | DLvValue of int | DLvTuple of ilvalue list
  type index_lvalue = (ilvalue, ty) typed

  type index_let_binding = {
    rec_f : rec_flag;
    l_v : index_lvalue;
    body : index_let_body;
  }

  and index_let_body = { lets : index_let_binding list; expr : index_expr }

  and d_expr =
    | DApply of index_expr * index_expr
    | DLiteral of literal
    | DValue of int
    | DFun of index_fun_body
    | DTuple of index_expr list
    | DIfElse of tif_else

  and index_fun_body = { lvalue : index_lvalue; b : index_let_body }
  and tif_else = { cond : index_expr; t_body : index_expr; f_body : index_expr }
  and index_expr = (d_expr, ty) typed

  type index_program = index_let_binding list

  module NameMap : sig
    type key = string
    type 'a t = 'a Map.Make(String).t

    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
  end

  val index_of_typed : int NameMap.t -> ty typ_program -> index_program
end
