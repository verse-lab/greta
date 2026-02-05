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
%left DASH%start topl10
%type <Ast.prog> topl10
%type <Ast.exp> ex8
%type <Ast.const> cons13
%%

cons13:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl12:
  | TINT id=iden11 EQ init=ex1  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex1:
  | e1=ex1 STAR e2=ex3  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | ex3  { $1 }
  ;

ex3:
  | e1=ex5 PLUS e2=ex3  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | ex5  { $1 }
  ;

ex5:
  | e1=ex5 DASH e2=ex5  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | ex8  { $1 }
  ;

ex8:
  | id=iden11  {  loc $startpos $endpos @@ Id (id)  }
  | c=cons13  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex1 RPAREN  {  e  }
  ;

iden11:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt2:
  | IF LPAREN e=ex1 RPAREN s1=stmt2  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | stmt4  { $1 }
  ;

stmt4:
  | IF LPAREN e=ex1 RPAREN s1=stmt4 ELSE s2=stmt4  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | stmt7  { $1 }
  ;

stmt6:
  | WHILE LPAREN e=ex1 RPAREN s=stmt6  {  loc $startpos $endpos @@ While(e, [s])  }
  | LBRACE ss=stmt9 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex1 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  ;

stmt7:
  | stmt6  {  $1  }
  | id=iden11 EQ e=ex1 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | d=decl12 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  ;

stmt9:
  | s=stmt2 ss=stmt9  {  s::ss  }
  |   {  []  }
  ;

topl10:
  | p=stmt9 EOF  {  p  }
  ;
