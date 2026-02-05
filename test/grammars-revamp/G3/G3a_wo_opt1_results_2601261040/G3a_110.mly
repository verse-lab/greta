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
%type <Ast.exp> ex9
%type <Ast.const> cons14
%%

cons14:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl13:
  | TINT id=iden11 EQ init=ex7  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex12:
  | exp23  {  $1  }
  ;

ex7:
  | e1=ex7 DASH e2=ex7  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | ex9  { $1 }
  ;

ex9:
  | ex12  { $1 }
  | e1=ex9 PLUS e2=exp23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  ;

exp23:
  | e1=exp23 STAR e2=exp26  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | exp26  { $1 }
  ;

exp26:
  | LPAREN e=ex7 RPAREN  {  e  }
  | id=iden11  {  loc $startpos $endpos @@ Id (id)  }
  | c=cons14  {  loc $startpos $endpos @@ Const (c)  }
  ;

iden11:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt1:
  | stmt2  { $1 }
  | IF LPAREN e=ex7 RPAREN s1=stmt1 ELSE s2=stmt1  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  ;

stmt2:
  | IF LPAREN e=ex7 RPAREN s1=stmt2  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | stmt5  { $1 }
  ;

stmt4:
  | RETURN e=ex7 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt8 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | WHILE LPAREN e=ex7 RPAREN s=stmt4  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt5:
  | stmt4  {  $1  }
  | d=decl13 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | id=iden11 EQ e=ex7 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  ;

stmt8:
  | s=stmt1 ss=stmt8  {  s::ss  }
  |   {  []  }
  ;

topl10:
  | p=stmt8 EOF  {  p  }
  ;
