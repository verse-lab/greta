%left PLUS
%left MUL

/* *** Grammar_0 *** */
%start program
%%

program : expr EOF { $1 };

cond_expr:
  | TRUE { Bool true }
  | FALSE { Bool false } 
  ;

expr:
  | INT  { Int $1 }
  | expr PLUS expr { Plus ($1, $3) }
  | expr MUL expr { Mul ($1, $3) }
  | LPAREN expr RPAREN { Paren $2 }
  | IF cond_expr THEN expr { If ($2, Then ($4, Else Na)) }
  | IF cond_expr THEN expr ELSE expr { If ($2, Then ($4, Else $6)) }
  ;

%%

(* 
  User Selection =>   
  [ ] ( ( IF c THEN e ELSE e ) + e )    
  [X] ( IF c THEN e ELSE ( e + e ) )
*)

/* *** Grammar_1 *** */
%start program
%%

program : expr1 EOF { $1 };

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

expr1:
  | IF cond_expr1 THEN expr1 { If ($2, Then ($4, Else Na)) }
  | IF cond_expr1 THEN expr1 ELSE expr1 { If ($2, Then ($4, Else Na)) }
  | expr1 MUL expr1 { Mul ($1, $3) }
  | expr2 { $1 }
  ;

expr2:
  | expr2 PLUS expr2 { Plus ($1, $3) }
  | LPAREN expr1 RPAREN { Paren $2 }
  | INT { Int $1 }
  ;

%%

(* 
  User Selection =>   
  [ ] ( ( IF c THEN e ELSE e ) * e )    
  [X] ( IF c THEN e ELSE ( e * e ) )
*)

/* *** Grammar_2 *** */
%start program
%%

program : expr1 EOF { $1 };

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

expr1:
  | IF cond_expr1 THEN expr1 ELSE expr1 { If ($2, Then ($4, Else Na)) }
  | IF cond_expr1 THEN expr1 { If ($2, Then ($4, Else Na)) }
  | expr2 { $1 }
  ;

expr2:
  | expr2 MUL expr2 { Mul ($1, $3) }
  | expr6 { $1 }
  ;

expr6:
  | expr6 PLUS expr6 { Plus ($1, $3) }
  | LPAREN expr1 RPAREN { Paren $2 }
  | INT { Int $1 }
  ;
%%

(*
  User Selection =>
  [X] (IF c THEN ( IF c THEN e ) ELSE e) 
  [ ] (IF c THEN ( IF c THEN e ELSE e ))
*)

/* *** Grammar_3 : resolved *** */
%start program
%%

program : expr1 EOF { $1 };

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

expr1:
  | IF cond_expr1 THEN expr2 ELSE expr1 { If ($2, Then ($4, Else Na)) }
  | expr3 { $1 }
  ;

expr2:
  | IF cond_expr1 THEN expr2 { If ($2, Then ($4, Else Na)) }
  | expr3 { $1 }
  ;

expr3:
  | expr3 MUL expr3 { Mul ($1, $3) }
  | expr6 { $1 }
  ;

expr6:
  | expr6 PLUS expr6 { Plus ($1, $3) }
  | LPAREN expr1 RPAREN { Paren $2 }
  | INT { Int $1 }
  ;
%%






