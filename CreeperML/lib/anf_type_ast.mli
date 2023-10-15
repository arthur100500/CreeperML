(*module AnfTypeAst : sig
    open Type_ast.TypeAst
    open Parser_ast.ParserAst
    open Position.Position

    (* Typed AST in ANF and De breujn indices *)
    type ilvalue = ILvUnit | ILvValue of int | ILvTuple of ilvalue list | ILvAny
    type db_name = ilvalue typed [@@deriving show { with_path = false }]
    type imm = ImmVal of int typed | ImmLit of literal typed

    (* Exprs with simple apply, tuples, vals and literals, ite constructions *)
    type anf_expr =
      | AApply of imm * imm
      | ATuple of imm list
      | AITE of imm * imm * imm
    [@@deriving show { with_path = false }]

    (* Let wit arguments and expr (name, args, res)*)
    type anf_let = { name : int typed; expr : anf_expr }
    [@@deriving show { with_path = false }]

    type fun_let = {
      is_rec : bool;
      name : int typed;
      arg : db_name;
      lets : let_binding list;
      res : imm;
    }
    [@@deriving show { with_path = false }]

    and let_binding = Function of fun_let | Binding of anf_let
    [@@deriving show { with_path = false }]

    val anf_of_program : typ_program -> let_binding list
    val show_anf_program : let_binding list -> string

    (* shows *)
    val show_ilvalue : ilvalue -> string
    val show_db_name : db_name -> string
    val show_imm : imm -> string
    val show_anf_expr : anf_expr -> string
    val show_anf_let : anf_let -> string
    val show_fun_let : fun_let -> string
    val show_let_binding : let_binding -> string
  end
*)
