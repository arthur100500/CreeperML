module ClosureAst : sig
  open Type_ast.TypeAst
  open Parser_ast.ParserAst

  type cf_typ_let_binding = {
    rec_f : rec_flag;
    l_v : ty typ_lvalue;
    cf_body : cf_typ_let_body;
  }

  and cf_typ_let_body = {
    cf_lets : cf_typ_let_binding list;
    cf_expr : cf_typ_expr;
  }

  and typ_fun_let_binding = {
    is_rec : rec_flag;
    name : (name, ty) typed;
    args : ty typ_lvalue list;
    b : cf_typ_let_body;
    env_vars : (name, ty) typed list;
  }

  and cf_expr =
    | CFApply of cf_typ_expr * cf_typ_expr
    | CFLiteral of literal
    | CFValue of name
    | CFTuple of cf_typ_expr list
    | CFIfElse of cf_if_else

  and cf_if_else = {
    cond : cf_typ_expr;
    t_body : cf_typ_expr;
    f_body : cf_typ_expr;
  }

  and cf_typ_expr = (cf_expr, ty) typed

  type cf_binding =
    | FunBinding of typ_fun_let_binding
    | ValBinding of cf_typ_let_binding

  type cf_typ_program = cf_binding list

  (* shows *)
  val show_cf_typ_let_binding : cf_typ_let_binding -> string
  val show_cf_typ_let_body : cf_typ_let_body -> string
  val show_typ_fun_let_binding : typ_fun_let_binding -> string
  val show_cf_expr : cf_expr -> string
  val show_cf_if_else : cf_if_else -> string
  val show_cf_typ_expr : cf_typ_expr -> string
  val show_cf_binding : cf_binding -> string
  val show_cf_typ_program : cf_typ_program -> string
  val show_cf_program : cf_typ_program -> string
end

module ClosureConvert : sig
  open ClosureAst
  open Type_ast.TypeAst

  val cf_program : ty typ_program -> (string, ty) typed list -> cf_typ_program
end
