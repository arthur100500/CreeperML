(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserAst = struct
    type name = string
    type binop = string
    type unop = string
    type arg = string
  
    type letBinding = 
    (* is recursive, name, body *)
    | LetBinding of bool * name * body
    (* let () = ... *)
    | LetUnit of body
  
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
    | Fun of arg list * body
  
    type program = letBinding list
  end
  