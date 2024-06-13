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
  User Selection =>     [X] 
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
  | expr1 MUL expr1 { Mul ($1, $3) }
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | IF cond_expr1 THEN expr1 { If ($2, Then ($4, Else Na)) }
  | expr2 { $1 }
  ;

expr2:
  | IF cond_expr1 THEN expr2 ELSE expr2 { If ($2, Then ($4, Else Na)) }
  | LPAREN expr1 RPAREN { Paren $2 }
  | INT { Int $1 }
  ;
%%

(* 
  User Selection =>     [X] ( ( e * e ) + e )    [ ] ( e * ( e + e ) )
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
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | IF cond_expr1 THEN expr1 { If ($2, Then ($4, Else Na)) }
  | expr2 { $1 }
  ;

expr2:
  | expr2 MUL expr2 { Mul ($1, $3) }
  | expr3 { $1 }
  ;

expr3:
  | IF cond_expr1 THEN expr3 ELSE expr3 { If ($2, Then ($4, Else Na)) }
  | LPAREN expr1 RPAREN { Paren $2 }
  | INT { Int $1 }
  ;

%%

(* 
  User Selection =>     [X] ( ( e + e ) + e )    [ ] ( e + ( e + e ) )

  (added) %left PLUS
*)

(*
  User Selection =>    [X] ( ( IF c THEN e ) + e )   [ ] ( IF c THEN ( e + e ) ) 
*)

%start program
%%

program : expr1 EOF { $1 };

cond_expr1:
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

expr1:
  | expr1 PLUS expr1 { Plus ($1, $3) }
  | expr2 { $1 }
  ;

expr2:
  | IF cond_expr1 THEN expr2 { If ($2, Then ($4, Else Na)) }
  | expr6 { $1 }
  ;

expr6:
  | expr6 MUL expr6 { Mul ($1, $3) }
  | expr10 { $1 }
  ;

expr10:
  | LPAREN expr1 RPAREN { Paren $2 }
  | INT { Int $1 }
  | IF cond_expr1 THEN expr10 ELSE expr10 { If ($2, Then ($4, Else Na)) }
  ;
%%



