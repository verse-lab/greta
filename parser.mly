%token <int> INT

%token PLUS
%token MUL

%token LPAREN
%token RPAREN

%token EOF

%type <Ast.t> program

%start program
%%

program : expr EOF { $1 };

expr: 
  | INT  { Ast.Int $1 }
  | expr PLUS expr { Ast.Plus ($1, $3) }
  | expr MUL expr { Ast.Mul ($1, $3) }
  | LPAREN expr RPAREN { Ast.Paren $2 }
  ;

/* 
program : expr_x EOF { $1 };

expr_x: 
  | expr_x PLUS expr_x { Ast.Plus ($1, $3) }
  | expr_y { $1 }
  ;

expr_y:
  | INT { Ast.Int $1 }
  | expr_y MUL expr_y { Ast.Mul ($1, $3) }
  | LPAREN expr_x RPAREN { Ast.Paren $2 }
  ;
 */