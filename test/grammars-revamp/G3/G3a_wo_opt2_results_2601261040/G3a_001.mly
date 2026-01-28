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
%type <Ast.exp> ex12
%type <Ast.const> cons15
%%

cons15:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl18:
  | TINT id=iden17 EQ init=ex8  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex10:
  | ex11  { $1 }
  ;

ex11:
  | e1=ex8 PLUS e2=exp24  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | ex12  { $1 }
  ;

ex12:
  | e1=ex12 DASH e2=ex12  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | exp24  {  $1  }
  ;

ex8:
  | ex10  { $1 }
  ;

exp23:
  | exp24  { $1 }
  ;

exp24:
  | e1=exp26 STAR e2=exp23  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | exp26  { $1 }
  ;

exp26:
  | LPAREN e=ex8 RPAREN  {  e  }
  | id=iden16  {  loc $startpos $endpos @@ Id (id)  }
  | c=cons15  {  loc $startpos $endpos @@ Const (c)  }
  ;

iden16:
  | iden17  { $1 }
  ;

iden17:
  | iden19  { $1 }
  ;

iden19:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt1:
  | stmt2  { $1 }
  ;

stmt13:
  | stmt14  { $1 }
  ;

stmt14:
  |   {  []  }
  | s=stmt2 ss=stmt13  {  s::ss  }
  ;

stmt2:
  | IF LPAREN e=ex11 RPAREN s1=stmt1  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | stmt5  { $1 }
  ;

stmt5:
  | stmt9  {  $1  }
  | d=decl18 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | id=iden19 EQ e=ex11 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | IF LPAREN e=ex10 RPAREN s1=stmt5 ELSE s2=stmt5  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  ;

stmt7:
  | stmt9  { $1 }
  ;

stmt9:
  | RETURN e=ex8 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt13 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | WHILE LPAREN e=ex8 RPAREN s=stmt7  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

topl1:
  | p=stmt14 EOF  {  p  }
  ;
