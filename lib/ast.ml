(* source: StackOverflow
 * https://stackoverflow.com/questions/910445/issue-resolving-a-shift-reduce-conflict-in-my-grammar 
 *)
 type t =
 | Var
 | And of t * t
 | Or  of t * t
 | Not of t
 | Paren of t 

