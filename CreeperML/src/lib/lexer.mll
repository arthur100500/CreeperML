{
    open Parser
}

let digit = ['0'-'9']
let sign = ['-' '+']

let int_const = sign? digit+

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
    | whitespace { token lexbuf }
    | int_const { INT (int_of_string @@ Lexing.lexeme lexbuf) }
    | ')' { RIGHTPARENT }
    | '(' { LEFTPARENT }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { ASTERISK }
    | '/' { SLASH }
    | eof { EOF }
    | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }