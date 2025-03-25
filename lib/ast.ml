type 'a loc = {
    elt : 'a
  ; loc : Range.t
}

let no_loc x = {elt=x; loc=Range.norange}

type id =  string loc (* Identifiers *)

type _const =
  | CInt  of int64
and const = _const loc

type binop =
  | Add | Sub | Mul

and _exp =  
  | Id of id
  | Const of const 
  | Bop of binop * exp * exp 
and exp = _exp loc

type _stmt =
  | Decl of decl
  | Assn of id * exp
  | If of exp * block * block  
  | While of exp * block
  | Ret of exp
  | Block of block
and stmt = _stmt loc

and _decl = {id : id; init : exp;}
and decl = _decl loc

and block = stmt list

type prog = block