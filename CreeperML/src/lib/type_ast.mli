(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module InferType : sig
  open Parser_ast.ParserAst

  (* inner types for inferencer that unused in other modules *)

  (* level of type nesting *)
  type lvl = int

  (* minimal type of expression *)
  and ground_typ = TInt | TString | TBool | TUnit | TFloat

  (* complex type *)
  type ty =
    | TArrow of typ * typ
    | TTuple of typ list
    | TGround of ground_typ
    | TVar of tv ref

  (* subtype for infer *)
  and tv = Unbound of name * lvl | Link of typ

  (* levels of type nesting (for infer) *)
  and 'a lvls = { value : 'a; mutable old_lvl : lvl; mutable new_lvl : lvl }

  (* main type for infering *)
  and typ = ty lvls

  (* environment *)
  type env = (name * typ) list

  (* shows *)
  val show_lvl : lvl -> string
  val show_ground_typ : ground_typ -> string
  val show_ty : ty -> string
  val show_tv : tv -> string
  val show_lvls : (Format.formatter -> 'a -> unit) -> 'a lvls -> string
  val show_typ : typ -> string

  (* pps *)
  val pp_lvl : Format.formatter -> lvl -> unit
  val pp_ground_typ : Format.formatter -> ground_typ -> unit
  val pp_ty : Format.formatter -> ty -> unit
  val pp_tv : Format.formatter -> tv -> unit

  val pp_lvls :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a lvls -> unit

  val pp_typ : Format.formatter -> typ -> unit
end

module InferTypeUtils : sig
  open Parser_ast.ParserAst
  open InferType

  val t_int : ground_typ
  val t_string : ground_typ
  val t_bool : ground_typ
  val t_unit : ground_typ
  val t_arrow : typ -> typ -> ty
  val t_tuple : typ list -> ty
  val t_ground : ground_typ -> ty
  val t_var : tv ref -> ty
  val tv_unbound : name -> lvl -> tv
  val tv_link : typ -> tv
  val is_unit : typ -> bool
  val lvl_value : 'a lvls -> 'a
  val with_lvls : lvl -> lvl -> 'a -> 'a lvls

  (* find in env *)
  val assoc : name -> env -> typ

  (* get type of const *)
  val convert_const : literal Position.Position.position -> ty

  (* simplifies links *)
  val repr : typ -> typ
end

module TypeAst : sig
  open InferType
  open Parser_ast.ParserAst

  (* outter typed ast that used by next modules *)

  (* types without lvlvs *)
  type ty =
    | TyArrow of ty * ty
    | TyTuple of ty list
    | TyGround of ground_typ
    | TyVar of name

  (* expresion with his type *)
  and 'a typed = { value : 'a; typ : ty }

  type typ_lvalue = lvalue typed

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
  and typ_expr = t_expr typed

  type typ_program = typ_let_binding list

  (* shows *)
  val show_ty : ty -> string
  val show_typed : (Format.formatter -> 'a -> unit) -> 'a typed -> string
  val show_typ_lvalue : typ_lvalue -> string
  val show_typ_let_binding : typ_let_binding -> string
  val show_typ_let_body : typ_let_body -> string
  val show_tfun_body : tfun_body -> string
  val show_tif_else : tif_else -> string
  val show_t_expr : t_expr -> string
  val show_typ_expr : typ_expr -> string
  val show_typ_program : typ_program -> string

  (* pps *)
  val pp_ty : Format.formatter -> ty -> unit

  val pp_typed :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a typed -> unit

  val pp_typ_lvalue : Format.formatter -> typ_lvalue -> unit
  val pp_typ_let_binding : Format.formatter -> typ_let_binding -> unit
  val pp_typ_let_body : Format.formatter -> typ_let_body -> unit
  val pp_tfun_body : Format.formatter -> tfun_body -> unit
  val pp_tif_else : Format.formatter -> tif_else -> unit
  val pp_t_expr : Format.formatter -> t_expr -> unit
  val pp_typ_expr : Format.formatter -> typ_expr -> unit
  val pp_typ_program : Format.formatter -> typ_program -> unit
end

module TypeAstUtils : sig
  open Parser_ast.ParserAst
  open TypeAst

  val ty_arrow : ty -> ty -> ty
  val ty_tuple : ty list -> ty
  val ty_ground : InferType.ground_typ -> ty
  val ty_var : name -> ty
  val typed_value : 'a typed -> 'a
  val with_typ : ty -> 'a -> 'a typed
  val typ : 'a typed -> ty

  val typ_let_binding :
    rec_flag -> typ_lvalue -> typ_let_body -> typ_let_binding

  val typ_let_body : typ_let_binding list -> typ_expr -> typ_let_body
  val t_apply : typ_expr -> typ_expr -> t_expr
  val t_literal : literal -> t_expr
  val t_value : name -> t_expr
  val t_fun : typ_lvalue -> typ_let_body -> t_expr
  val t_tuple : typ_expr list -> t_expr
  val t_if_else : typ_expr -> typ_expr -> typ_expr -> t_expr

  (* removes infer's levels from types *)
  val remove_lvl : InferType.typ -> ty
end
