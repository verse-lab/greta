type expr =
  | EVar of string
  | EArrow of expr * expr 
  | EChain of expr * expr * expr * expr * expr 

