<<<<<<< HEAD
type ty = TyString of string 

type id = IdString of string

type expr = Call of id

type param = Param of ty 

type t =
| Decl1 of ty * id * expr
| Decl2 of ty * id * param
=======
type 'a loc = {
    elt : 'a
  ; loc : Range.t
}
>>>>>>> e3802eee547e5493584a32946f0dfc57ba186b3d

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