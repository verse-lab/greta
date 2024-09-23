/* *** G3c *** */
// 14 conflicts - 14 po's 0 assoc
// ~ vs. *, +, ==, -2 (4)
// -1 vs. *, +, (, {, ==, -2 (6)
// ! vs. *, +, ==, -2 (4)

%{
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}

/* Declare your tokens here. */
%token EOF
%token <int64>  INT
%token NULL
%token <string> STRING
%token <string> IDENT

%token TINT     /* int */
%token TVOID    /* void */
%token TSTRING  /* string */
%token IF       /* if */
%token ELSE     /* else */
%token WHILE    /* while */
%token RETURN   /* return */
%token VAR      /* var */
%token SEMI     /* ; */
%token COMMA    /* , */
%token LBRACE   /* { */
%token RBRACE   /* } */
%token PLUS     /* + */
%token DASH     /* - */
%token STAR     /* * */
%token EQEQ     /* == */
%token EQ       /* = */
%token LPAREN   /* ( */
%token RPAREN   /* ) */
%token LBRACKET /* [ */
%token RBRACKET /* ] */
%token TILDE    /* ~ */
%token BANG     /* ! */
%token GLOBAL   /* global */


%nonassoc BANG
%nonassoc TILDE
%nonassoc LBRACKET
%nonassoc LPAREN

/* ---------------------------------------------------------------------- */

%start prog
%start exp_top
%start stmt_top
%type <Ast.exp Ast.node> exp_top
%type <Ast.stmt Ast.node> stmt_top

%type <Ast.prog> prog
%type <Ast.exp Ast.node> exp1
%type <Ast.exp Ast.node> exp2
%type <Ast.stmt Ast.node> stmt
%type <Ast.block> block
%type <Ast.ty> ty
%%

exp_top:
  | e=exp1 EOF { e }

stmt_top:
  | s=stmt EOF { s }

prog:
  | p=list(decl) EOF  { p }

decl:
  | GLOBAL name=IDENT EQ init=gexp SEMI
    { Gvdecl (loc $startpos $endpos { name; init }) }
  | frtyp=ret_ty fname=IDENT LPAREN args=arglist RPAREN body=block
    { Gfdecl (loc $startpos $endpos { frtyp; fname; args; body }) }

arglist:
  | l=separated_list(COMMA, pair(ty,IDENT)) { l }
    
ty:
  | TINT   { TInt }
  | r=rtyp { TRef r } 


%inline ret_ty:
  | TVOID  { RetVoid }
  | t=ty   { RetVal t }

%inline rtyp:
  | TSTRING { RString }
  | t=ty LBRACKET RBRACKET { RArray t }

%inline bop:
  | PLUS   { Add }
  | DASH   { Sub }
  | STAR   { Mul }
  | EQEQ   { Eq } 

%inline uop:
  | DASH  { Neg }
  | BANG  { Lognot }
  | TILDE { Bitnot }

gexp:
  | t=rtyp NULL  { loc $startpos $endpos @@ CNull t }
  | i=INT      { loc $startpos $endpos @@ CInt i } 

lhs:  
  | id=IDENT            { loc $startpos $endpos @@ Id id }
  | e=exp1 LBRACKET i=exp1 RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }

exp1:
  | exp2 { $1 }
  | e1=exp1 b=bop e2=exp2 { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | id=IDENT            { loc $startpos $endpos @@ Id id }
  | e=exp1 LBRACKET i=exp1 RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }
  | e=exp1 LPAREN es=separated_list(COMMA, exp1) RPAREN
                        { loc $startpos $endpos @@ Call (e,es) }
  | t=rtyp NULL           { loc $startpos $endpos @@ CNull t }
  | u=uop e=exp1         { loc $startpos $endpos @@ Uop (u, e) }

exp2:
  | i=INT               { loc $startpos $endpos @@ CInt i }
  | LPAREN e=exp1 RPAREN { e }
  

vdecl:
  | VAR id=IDENT EQ init=exp1 { (id, init) }

stmt: 
  | d=vdecl SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=lhs EQ e=exp1 SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=exp1 LPAREN es=separated_list(COMMA, exp1) RPAREN SEMI
                        { loc $startpos $endpos @@ SCall (e, es) }
  | ifs=if_stmt         { ifs }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=exp1 SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp1 RPAREN b=block  
                        { loc $startpos $endpos @@ While(e, b) } 

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF LPAREN e=exp1 RPAREN b1=block b2=else_stmt
    { loc $startpos $endpos @@ If(e,b1,b2) }

else_stmt:
  | (* empty *)       { [] }
  | ELSE b=block      { b }
  | ELSE ifs=if_stmt  { [ ifs ] }

