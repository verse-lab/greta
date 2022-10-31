%token <int> INT
%token <bool> BOOL

%token PLUS
%token MUL

%token LPAREN
%token RPAREN

%token IF
%token THEN
%token ELSE

%token EOF

%type <Ast.t> program

%start program
%%

program : expr EOF { $1 };

cond_expr: BOOL { Ast.Bool $1 }

expr: 
  | INT  { Ast.Int $1 }
  | expr PLUS expr { Ast.Plus ($1, $3) }
  | expr MUL expr { Ast.Mul ($1, $3) }
  | LPAREN expr RPAREN { Ast.Paren $2 }
  | BOOL { Ast.Bool $1 }
  | IF cond_expr THEN expr { Ast.If1 ($2, $4) }
  | IF cond_expr THEN expr ELSE expr { Ast.If2 ($2, $4, $6) };

