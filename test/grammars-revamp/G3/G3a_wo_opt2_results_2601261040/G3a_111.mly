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
%type <Ast.exp> ex13
%type <Ast.const> cons16
%%

cons16:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl19:
  | TINT id=iden18 EQ init=ex9  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex11:
  | ex12  { $1 }
  ;

ex12:
  | ex13  { $1 }
  | e1=ex9 DASH e2=ex9  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  ;

ex13:
  | e1=ex13 PLUS e2=exp24  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | exp25  {  $1  }
  ;

ex9:
  | ex11  { $1 }
  ;

exp23:
  | exp24  { $1 }
  ;

exp24:
  | exp25  { $1 }
  ;

exp25:
  | exp27  { $1 }
  | e1=exp27 STAR e2=exp23  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  ;

exp27:
  | c=cons16  {  loc $startpos $endpos @@ Const (c)  }
  | id=iden17  {  loc $startpos $endpos @@ Id (id)  }
  | LPAREN e=ex9 RPAREN  {  e  }
  ;

iden17:
  | iden18  { $1 }
  ;

iden18:
  | iden20  { $1 }
  ;

iden20:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt1:
  | stmt2  { $1 }
  ;

stmt10:
  | WHILE LPAREN e=ex9 RPAREN s=stmt8  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex9 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt14 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  ;

stmt14:
  | stmt15  { $1 }
  ;

stmt15:
  |   {  []  }
  | s=stmt2 ss=stmt14  {  s::ss  }
  ;

stmt2:
  | IF LPAREN e=ex11 RPAREN s1=stmt1 ELSE s2=stmt1  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | stmt6  { $1 }
  ;

stmt6:
  | IF LPAREN e=ex12 RPAREN s1=stmt6  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | stmt10  {  $1  }
  | d=decl19 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | id=iden20 EQ e=ex11 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  ;

stmt8:
  | stmt10  { $1 }
  ;

topl1:
  | p=stmt15 EOF  {  p  }
  ;
