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
%left DASH%start topl26
%type <Ast.prog> topl26
%type <Ast.exp> ex6
%type <Ast.const> cons54
%%

cons45:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons46:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons47:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons48:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons49:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons50:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons51:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons52:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons53:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons54:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl36:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl37:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl38:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl39:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl40:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl41:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl42:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl43:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl44:
  | TINT id=iden33 EQ init=ex4  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex1:
  | e1=ex10 STAR e2=ex12  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex4 RPAREN  {  e  }
  | e1=ex4 DASH e2=ex4  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  | e1=ex12 PLUS e2=ex23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  ;

ex10:
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  | e1=ex12 PLUS e2=ex23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | LPAREN e=ex4 RPAREN  {  e  }
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  | e1=ex10 STAR e2=ex12  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  ;

ex12:
  | e1=ex12 PLUS e2=ex23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex4 RPAREN  {  e  }
  ;

ex2:
  | e1=ex12 PLUS e2=ex23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  | e1=ex10 STAR e2=ex12  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | e1=ex4 DASH e2=ex4  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex4 RPAREN  {  e  }
  ;

ex23:
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex4 RPAREN  {  e  }
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  ;

ex3:
  | LPAREN e=ex4 RPAREN  {  e  }
  | e1=ex4 DASH e2=ex4  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  | e1=ex12 PLUS e2=ex23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  | e1=ex10 STAR e2=ex12  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  ;

ex4:
  | e1=ex4 DASH e2=ex4  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | e1=ex10 STAR e2=ex12  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | LPAREN e=ex4 RPAREN  {  e  }
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  | e1=ex12 PLUS e2=ex23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  ;

ex5:
  | e1=ex10 STAR e2=ex12  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | e1=ex4 DASH e2=ex4  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | e1=ex12 PLUS e2=ex23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  | LPAREN e=ex4 RPAREN  {  e  }
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  ;

ex6:
  | LPAREN e=ex4 RPAREN  {  e  }
  | id=iden33  {  loc $startpos $endpos @@ Id (id)  }
  | e1=ex4 DASH e2=ex4  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | c=cons51  {  loc $startpos $endpos @@ Const (c)  }
  | e1=ex12 PLUS e2=ex23  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | e1=ex10 STAR e2=ex12  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  ;

iden27:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden28:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden29:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden30:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden31:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden32:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden33:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden34:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden35:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt11:
  | IF LPAREN e=ex2 RPAREN s1=stmt11  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | stmt13  {  $1  }
  | id=iden27 EQ e=ex1 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | d=decl36 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  ;

stmt13:
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  ;

stmt14:
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  ;

stmt15:
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  ;

stmt16:
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  ;

stmt17:
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt18:
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  ;

stmt19:
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt20:
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt21:
  | LBRACE ss=stmt25 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex4 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex4 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt22:
  | id=iden27 EQ e=ex1 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | d=decl36 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | stmt13  {  $1  }
  ;

stmt24:
  |   {  []  }
  | s=stmt9 ss=stmt25  {  s::ss  }
  ;

stmt25:
  |   {  []  }
  | s=stmt9 ss=stmt25  {  s::ss  }
  ;

stmt7:
  | id=iden27 EQ e=ex1 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | IF LPAREN e=ex1 RPAREN s1=stmt7 ELSE s2=stmt7  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | stmt13  {  $1  }
  | d=decl36 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | IF LPAREN e=ex2 RPAREN s1=stmt11  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  ;

stmt8:
  | IF LPAREN e=ex2 RPAREN s1=stmt11  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | stmt13  {  $1  }
  | id=iden27 EQ e=ex1 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | d=decl36 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | IF LPAREN e=ex1 RPAREN s1=stmt7 ELSE s2=stmt7  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  ;

stmt9:
  | IF LPAREN e=ex1 RPAREN s1=stmt7 ELSE s2=stmt7  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | d=decl36 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | id=iden27 EQ e=ex1 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | IF LPAREN e=ex2 RPAREN s1=stmt11  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | stmt13  {  $1  }
  ;

topl26:
  | p=stmt24 EOF  {  p  }
  ;
