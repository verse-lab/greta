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

