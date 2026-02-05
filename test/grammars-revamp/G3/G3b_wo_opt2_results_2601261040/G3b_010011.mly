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
%type <Ast.exp> ex7
%type <Ast.const> cons13
%%

cons13:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl15:
  | TINT id=iden14 EQ init=ex1  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex1:
  | ex2  { $1 }
  ;

ex2:
  | ex3  { $1 }
  ;

ex3:
  | e1=ex4 PLUS e2=ex1  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | ex4  { $1 }
  ;

ex4:
  | ex7  { $1 }
  | e1=ex7 STAR e2=ex4  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  ;

ex7:
  | e1=ex7 DASH e2=ex7  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | c=cons13  {  loc $startpos $endpos @@ Const (c)  }
  | id=iden14  {  loc $startpos $endpos @@ Id (id)  }
  | LPAREN e=ex1 RPAREN  {  e  }
  ;

iden14:
  | iden16  { $1 }
  ;

iden16:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt10:
  | WHILE LPAREN e=ex1 RPAREN s=stmt9  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex1 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt11 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  ;

stmt11:
  | stmt12  { $1 }
  ;

stmt12:
  | s=stmt6 ss=stmt11  {  s::ss  }
  |   {  []  }
  ;

stmt5:
  | stmt6  { $1 }
  ;

stmt6:
  | stmt8  { $1 }
  | IF LPAREN e=ex2 RPAREN s1=stmt5  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  ;

stmt8:
  | stmt10  {  $1  }
  | id=iden16 EQ e=ex2 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | d=decl15 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | IF LPAREN e=ex3 RPAREN s1=stmt8 ELSE s2=stmt8  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  ;

stmt9:
  | stmt10  { $1 }
  ;

topl1:
  | p=stmt12 EOF  {  p  }
  ;
