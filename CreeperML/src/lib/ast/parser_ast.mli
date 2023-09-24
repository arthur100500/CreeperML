(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserAst : sig
    type name = string
    type binop = string
    type unop = string
    type arg = string
    type lvalue = lvalue list
  
    type letBinding = 
    (* is recursive, name, body *)
    | LetBinding of bool * lvalue * body
  
    (*
      let x = .... (in?)
      let x = .... (in?)
      expr  
    *)
    and body = letBinding list * expr
  
    and expr = 
    (* e e *)
    | Apply of expr * expr
    (* "among" or 90 *)
    | Literal of string
    (* let a = ...; a *)
    | Value of string
    (* fun x y z -> ... *)
    | Fun of lvalue list * body
    (* (a, b, c) *)
    | Tuple of expr list

  
    type program = letBinding list
  end
  
  