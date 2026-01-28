%{

open Ast;;

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a loc =
  { elt ; loc=Range.mk_lex_range startpos endpos }


%}
%token EOF
%token <int64> INT
%token <string> IDENT
%token <string> STRING
%token ELSE
%token IF
%token TINT
%token RETURN
%token WHILE
%token SEMI
%token LBRACE
%token RBRACE
%token PLUS
%token DASH
%token STAR
%token EQ
%token LPAREN
%token RPAREN
%left DASH%start topl1
%type <Ast.prog> topl1
%type <Ast.exp> ex4
%type <Ast.const> cons8
%%

cons8:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl9:
  | TINT id=iden10 EQ init=ex1  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex1:
  | e1=ex1 STAR e2=ex2  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | ex2  { $1 }
  ;

ex2:
  | ex4  { $1 }
  | e1=ex4 PLUS e2=ex2  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  ;

ex4:
  | e1=ex4 DASH e2=ex4  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | LPAREN e=ex1 RPAREN  {  e  }
  | c=cons8  {  loc $startpos $endpos @@ Const (c)  }
  | id=iden10  {  loc $startpos $endpos @@ Id (id)  }
  ;

iden10:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt3:
  | stmt5  { $1 }
  | IF LPAREN e=ex1 RPAREN s1=stmt3  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  ;

stmt5:
  | IF LPAREN e=ex1 RPAREN s1=stmt5 ELSE s2=stmt5  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | id=iden10 EQ e=ex1 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | d=decl9 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | stmt6  {  $1  }
  ;

stmt6:
  | WHILE LPAREN e=ex1 RPAREN s=stmt6  {  loc $startpos $endpos @@ While(e, [s])  }
  | LBRACE ss=stmt7 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex1 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  ;

stmt7:
  | s=stmt3 ss=stmt7  {  s::ss  }
  |   {  []  }
  ;

topl1:
  | p=stmt7 EOF  {  p  }
  ;
