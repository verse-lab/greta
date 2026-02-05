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
%type <Ast.const> cons9
%%

cons9:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl10:
  | TINT id=iden11 EQ init=ex6  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex6:
  | e1=ex7 PLUS e2=exp22  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | exp22  {  $1  }
  | e1=ex6 DASH e2=ex6  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  ;

ex7:
  | e1=ex7 PLUS e2=exp22  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | exp22  {  $1  }
  ;

exp22:
  | id=iden11  {  loc $startpos $endpos @@ Id (id)  }
  | e1=exp22 STAR e2=exp24  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | LPAREN e=ex6 RPAREN  {  e  }
  | c=cons9  {  loc $startpos $endpos @@ Const (c)  }
  ;

exp24:
  | id=iden11  {  loc $startpos $endpos @@ Id (id)  }
  | c=cons9  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex6 RPAREN  {  e  }
  ;

iden11:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt1:
  | d=decl10 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | stmt5  {  $1  }
  | IF LPAREN e=ex6 RPAREN s1=stmt1 ELSE s2=stmt1  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | id=iden11 EQ e=ex6 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | IF LPAREN e=ex6 RPAREN s1=stmt3  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  ;

stmt3:
  | d=decl10 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | stmt5  {  $1  }
  | IF LPAREN e=ex6 RPAREN s1=stmt3  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | id=iden11 EQ e=ex6 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  ;

stmt5:
  | RETURN e=ex6 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt8 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | WHILE LPAREN e=ex6 RPAREN s=stmt5  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt8:
  | s=stmt1 ss=stmt8  {  s::ss  }
  |   {  []  }
  ;

topl1:
  | p=stmt8 EOF  {  p  }
  ;
