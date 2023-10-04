(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserAst : sig
  type name = string
  type rec_flag = Rec | NoRec

  type lvalue =
    (* _ *)
    | LvAny
    (* () *)
    | LvUnit
    (* a *)
    | LvValue of name
    (* (a, b)
       invariant >= 2 *)
    | LvTuple of lvalue list

  type literal =
    | LInt of int
    | LFloat of float
    | LString of string
    | LBool of bool
    | LUnit

  type let_binding = {
    (* is recursive, name, body *)
    rec_f : rec_flag;
    l_v : lvalue;
    body : let_body;
  }

  (*
      let x = .... (in?)
      let x = .... (in?)
      expr  
    *)
  and let_body = { lets : let_binding list; expr : expr }

  and expr =
    (* e e *)
    | EApply of expr * expr
    (* "among" or 90 *)
    | ELiteral of literal
    (* a *)
    | EValue of name
    (* fun x -> fun y -> ...
       fun x y -> ... represented as EFun x {[], EFun y ...} *)
    | EFun of lvalue * let_body
    (* (a, b, c)
       invariant >= 2 *)
    | ETuple of expr list
    (* if else statement *)
    | EIfElse of if_else_body

  (* if condition then true_body else else_body *)
  and if_else_body = { cond : expr; t_body : expr; f_body : expr }

  type program = let_binding list

  (* shows *)
  val show_name : name -> string
  val show_rec_flag : rec_flag -> string
  val show_lvalue : lvalue -> string
  val show_literal : literal -> string
  val show_let_binding : let_binding -> string
  val show_let_body : let_body -> string
  val show_expr : expr -> string
  val show_program : program -> string
end

module ParserAstUtils : sig
  open ParserAst

  (* recursive flags *)
  val rec_f : rec_flag
  val norec_f : rec_flag

  (* left values *)
  val lv_any : lvalue
  val lv_unit : lvalue
  val lv_value : string -> lvalue
  val lv_tuple : lvalue list -> lvalue

  (* literals *)
  val l_int : int -> literal
  val l_float : float -> literal
  val l_string : string -> literal
  val l_bool : bool -> literal
  val l_unit : literal

  (* expressions *)
  val e_apply : expr -> expr -> expr
  val e_literal : literal -> expr
  val e_value : name -> expr
  val e_fun : lvalue -> let_body -> expr
  val e_tuple : expr list -> expr
  val e_if_else : expr -> expr -> expr -> expr

  (* lets *)
  val let_binding : ?rec_flag:rec_flag -> lvalue -> let_body -> let_binding
  val let_body : let_binding list -> expr -> let_body
end
