module ParserInterface : sig
  (* parse code from given string *)
  val from_string : string -> (Parser_ast.ParserAst.program, string) result

  (* parse code from given channel *)
  val from_channel : in_channel -> (Parser_ast.ParserAst.program, string) result

  (* parse code from file by given filename *)
  val from_file : string -> (Parser_ast.ParserAst.program, string) result
end
