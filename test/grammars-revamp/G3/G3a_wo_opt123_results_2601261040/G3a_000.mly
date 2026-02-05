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
%left DASH%start topl33
%type <Ast.prog> topl33
%type <Ast.exp> ex42
%type <Ast.const> cons61
%%

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

cons55:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons56:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons57:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons58:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons59:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons60:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

cons61:
  | i=INT  {  loc $startpos $endpos @@ CInt i  }
  ;

decl43:
  | TINT id=iden39 EQ init=ex27  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl44:
  | TINT id=iden39 EQ init=ex27  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl45:
  | TINT id=iden39 EQ init=ex27  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl46:
  | TINT id=iden39 EQ init=ex27  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl47:
  | TINT id=iden39 EQ init=ex27  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl48:
  | TINT id=iden39 EQ init=ex27  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl49:
  | TINT id=iden39 EQ init=ex27  {  loc $startpos $endpos @@ {id; init}  }
  ;

decl50:
  | TINT id=iden39 EQ init=ex27  {  loc $startpos $endpos @@ {id; init}  }
  ;

ex24:
  | e1=ex27 PLUS e2=exp211  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | e1=ex32 DASH e2=ex32  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | exp211  {  $1  }
  ;

ex25:
  | e1=ex32 DASH e2=ex32  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | e1=ex27 PLUS e2=exp211  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | exp211  {  $1  }
  ;

ex26:
  | exp211  {  $1  }
  | e1=ex27 PLUS e2=exp211  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | e1=ex32 DASH e2=ex32  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  ;

ex27:
  | e1=ex32 DASH e2=ex32  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | e1=ex27 PLUS e2=exp211  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | exp211  {  $1  }
  ;

ex28:
  | e1=ex32 DASH e2=ex32  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | exp211  {  $1  }
  | e1=ex27 PLUS e2=exp211  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  ;

ex29:
  | e1=ex27 PLUS e2=exp211  {  loc $startpos $endpos @@ Bop(Add, e1, e2)  }
  | e1=ex32 DASH e2=ex32  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | exp211  {  $1  }
  ;

ex32:
  | e1=ex32 DASH e2=ex32  {  loc $startpos $endpos @@ Bop(Sub, e1, e2)  }
  | exp211  {  $1  }
  ;

ex42:
  | exp211  {  $1  }
  ;

exp210:
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | LPAREN e=ex27 RPAREN  {  e  }
  ;

exp211:
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex27 RPAREN  {  e  }
  ;

exp212:
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex27 RPAREN  {  e  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  ;

exp213:
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | LPAREN e=ex27 RPAREN  {  e  }
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  ;

exp223:
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  | LPAREN e=ex27 RPAREN  {  e  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  ;

exp25:
  | LPAREN e=ex27 RPAREN  {  e  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  ;

exp26:
  | LPAREN e=ex27 RPAREN  {  e  }
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  ;

exp27:
  | LPAREN e=ex27 RPAREN  {  e  }
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  ;

exp28:
  | LPAREN e=ex27 RPAREN  {  e  }
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  ;

exp29:
  | LPAREN e=ex27 RPAREN  {  e  }
  | id=iden37  {  loc $startpos $endpos @@ Id (id)  }
  | e1=exp27 STAR e2=exp223  {  loc $startpos $endpos @@ Bop(Mul, e1, e2)  }
  | c=cons53  {  loc $startpos $endpos @@ Const (c)  }
  ;

iden34:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden35:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden36:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden37:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden38:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden39:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden40:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

iden41:
  | id=IDENT  {  loc $startpos $endpos id  }
  ;

stmt1:
  | stmt14  {  $1  }
  | id=iden34 EQ e=ex24 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | IF LPAREN e=ex24 RPAREN s1=stmt1  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | IF LPAREN e=ex25 RPAREN s1=stmt4 ELSE s2=stmt4  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | d=decl43 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  ;

stmt14:
  | LBRACE ss=stmt31 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex27 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex27 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt15:
  | LBRACE ss=stmt31 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex27 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex27 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt16:
  | LBRACE ss=stmt31 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex27 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex27 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt17:
  | LBRACE ss=stmt31 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | WHILE LPAREN e=ex27 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex27 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  ;

stmt18:
  | RETURN e=ex27 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt31 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | WHILE LPAREN e=ex27 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt19:
  | LBRACE ss=stmt31 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  | RETURN e=ex27 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex27 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  ;

stmt2:
  | d=decl43 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | stmt14  {  $1  }
  | IF LPAREN e=ex25 RPAREN s1=stmt4 ELSE s2=stmt4  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | id=iden34 EQ e=ex24 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | IF LPAREN e=ex24 RPAREN s1=stmt1  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  ;

stmt20:
  | WHILE LPAREN e=ex27 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  | RETURN e=ex27 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | LBRACE ss=stmt31 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  ;

stmt21:
  | RETURN e=ex27 SEMI  {  loc $startpos $endpos @@ Ret(e)  }
  | WHILE LPAREN e=ex27 RPAREN s=stmt19  {  loc $startpos $endpos @@ While(e, [s])  }
  | LBRACE ss=stmt31 RBRACE  {  loc $startpos $endpos @@ Block(ss)  }
  ;

stmt22:
  | d=decl43 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | stmt14  {  $1  }
  | id=iden34 EQ e=ex24 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  ;

stmt3:
  | IF LPAREN e=ex25 RPAREN s1=stmt4 ELSE s2=stmt4  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | stmt14  {  $1  }
  | IF LPAREN e=ex24 RPAREN s1=stmt1  {  loc $startpos $endpos @@ If(e, [s1], [])  }
  | d=decl43 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  | id=iden34 EQ e=ex24 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  ;

stmt30:
  | s=stmt3 ss=stmt31  {  s::ss  }
  |   {  []  }
  ;

stmt31:
  | s=stmt3 ss=stmt31  {  s::ss  }
  |   {  []  }
  ;

stmt4:
  | stmt14  {  $1  }
  | id=iden34 EQ e=ex24 SEMI  {  loc $startpos $endpos @@ Assn(id, e)  }
  | IF LPAREN e=ex25 RPAREN s1=stmt4 ELSE s2=stmt4  {  loc $startpos $endpos @@ If(e, [s1], [s2])  }
  | d=decl43 SEMI  {  loc $startpos $endpos @@ Decl(d)  }
  ;

topl33:
  | p=stmt30 EOF  {  p  }
  ;
