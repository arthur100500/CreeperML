module InferType : sig
  open Parser_ast.ParserAst

  (* level of type nesting *)
  type lvl = int

  (* minimal type of expression *)
  and ground_typ = TInt | TString | TBool | TUnit

  (* complex type *)
  type t =
    | TArrow of t * t
    | TTuple of t list
    | TGround of ground_typ
    | TVar of tv ref

  (* subtype for infer *)
  and tv = Unbound of name * lvl | Link of t

  (* expresion with his type *)
  and 'a typed = { value : 'a; typ : t }

  (* levels of type nesting (for infer) *)
  type 'a lvls = { value : 'a; mutable old_lvl : lvl; mutable new_lvl : lvl }

  (* main type for infering *)
  and typ = t lvls

  (* shows *)
  val show_lvl : lvl -> string
  val show_ground_typ : ground_typ -> string

  (* val show_t : t -> string *)
  val show_tv : tv -> string
  val show_typed : (Format.formatter -> 'a -> unit) -> 'a typed -> string
  val show_lvls : (Format.formatter -> 'a -> unit) -> 'a lvls -> string
  val show_typ : typ -> string

  (* pps *)
  val pp_lvl : Format.formatter -> lvl -> unit
  val pp_ground_typ : Format.formatter -> ground_typ -> unit

  (* val pp_t : Format.formatter -> t -> unit *)
  val pp_tv : Format.formatter -> tv -> unit

  val pp_typed :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a typed -> unit

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
  val t_arrow : t -> t -> t
  val t_tuple : t list -> t
  val t_ground : ground_typ -> t
  val t_var : tv ref -> t
  val tv_unbound : name -> lvl -> tv
  val tv_link : t -> tv
  val typed_value : 'a typed -> 'a
  val with_typ : t -> 'a -> 'a typed
  val typ : 'a typed -> t
  val lvl_value : 'a lvls -> 'a
  val with_lvls : lvl -> lvl -> 'a -> 'a lvls
end

module TypeAst : sig
  open InferType
  open Parser_ast.ParserAst

  type typ_name = name typed
  and typ_literal = literal typed
  and typ_lvalue = lvalue typed

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

  and typ_expr = t_expr typed

  type typ_program = typ_let_binding list

  (* shows *)
  val show_typ_name : typ_name -> string
  val show_typ_literal : typ_literal -> string
  val show_typ_lvalue : typ_lvalue -> string
  val show_typ_let_binding : typ_let_binding -> string
  val show_typ_let_body : typ_let_body -> string
  val show_t_expr : t_expr -> string
  val show_typ_expr : typ_expr -> string
  val show_typ_program : typ_program -> string

  (* pps *)
  val pp_typ_name : Format.formatter -> typ_name -> unit
  val pp_typ_literal : Format.formatter -> typ_literal -> unit
  val pp_typ_lvalue : Format.formatter -> typ_lvalue -> unit
  val pp_typ_let_binding : Format.formatter -> typ_let_binding -> unit
  val pp_typ_let_body : Format.formatter -> typ_let_body -> unit
  val pp_t_expr : Format.formatter -> t_expr -> unit
  val pp_typ_expr : Format.formatter -> typ_expr -> unit
  val pp_typ_program : Format.formatter -> typ_program -> unit
end

module TypeAstUtils : sig
  open Parser_ast.ParserAst
  open TypeAst

  val typ_let_binding :
    rec_flag -> typ_lvalue -> typ_let_body -> typ_let_binding

  val typ_let_body : typ_let_binding list -> typ_expr -> typ_let_body
  val t_apply : typ_expr -> typ_expr -> t_expr
  val t_literal : typ_literal -> t_expr
  val t_value : typ_name -> t_expr
  val t_fun : typ_lvalue -> typ_let_body -> t_expr
  val t_typle : typ_expr list -> t_expr
  val t_if_else : typ_expr -> typ_expr -> typ_expr -> t_expr
end
