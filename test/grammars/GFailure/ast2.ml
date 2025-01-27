type t =
  | Id
  | Num 
  | Numid
  | Web
  | Bracketquestion of seq
  | Bracket of string * seq * seq
  | Assign of t * t
and seq = t list