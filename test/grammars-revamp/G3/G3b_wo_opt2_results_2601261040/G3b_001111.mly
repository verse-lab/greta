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
%type <Ast.const> cons14
%%

cons14:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl16:
  | TINT id=iden15 EQ init=ex1  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex1:
  | ex2  { $1 }
  ;

ex10:
  | id=iden15  {  loc $startpos $endpos @@ Id (id)  }
  | c=cons14  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex1 RPAREN  {  e  }
  ;

ex2:
  | ex3  { $1 }
  ;

ex3:
  | e1=ex1 DASH e2=ex1  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | ex4  { $1 }
  ;

ex4:
  | ex7  { $1 }
  | e1=ex7 STAR e2=ex4  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  ;

ex7:
  | ex10  { $1 }
  | e1=ex10 PLUS e2=ex7  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  ;

iden15:
  | iden17  { $1 }
  ;

iden17:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt11:
  | LBRACE ss=stmt12 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | WHILE LPAREN e=ex1 RPAREN s=stmt9  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex1 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  ;

stmt12:
  | stmt13  { $1 }
  ;

stmt13:
  |   {  []  }
  | s=stmt6 ss=stmt12  {  s::ss  }
  ;

stmt5:
  | stmt6  { $1 }
  ;

stmt6:
  | stmt8  { $1 }
  | IF LPAREN e=ex2 RPAREN s1=stmt5  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  ;

stmt8:
  | stmt11  {  $1  }
  | id=iden17 EQ e=ex2 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | d=decl16 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | IF LPAREN e=ex3 RPAREN s1=stmt8 ELSE s2=stmt8  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  ;

stmt9:
  | stmt11  { $1 }
  ;

topl1:
  | p=stmt13 EOF  {  p  }
  ;
