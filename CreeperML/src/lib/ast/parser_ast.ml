(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserAst = struct
  type name = string
  type binop = string
  type unop = string
  type arg = string

  type letBinding = 
  | LetBinding of bool * name * body
  | LetUnit of body

  and body = letBinding list * expr

  and expr = 
  | Apply of expr * expr
  | Literal of string
  | Value of string
  | Fun of arg list * body

  type program = letBinding list
end
