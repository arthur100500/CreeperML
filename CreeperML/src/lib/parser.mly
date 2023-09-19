%{
    open Ast
%}

%token <int> INT
%token RIGHTPARENT
%token LEFTPARENT
%token PLUS
%token MINUS
%token ASTERISK
%token SLASH

%token EOF

%type <Ast.expr> parse

%start parse

%%

parse : e = expr; EOF { e }

expr : 
    | n = INT { eint n }
    | e1 = expr; PLUS; e2 = expr { esum e1 e2 }
    | e1 = expr; MINUS; e2 = expr { esub e1 e2 }
    | e1 = expr; ASTERISK; e2 = expr { emul e1 e2 }
    | e1 = expr; SLASH; e2 = expr { ediv e1 e2 }
    | LEFTPARENT; e = expr; RIGHTPARENT { e }