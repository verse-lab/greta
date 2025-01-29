type 'a loc = {
    elt : 'a
  ; loc : Range.t
}

let no_loc x = {elt=x; loc=Range.norange}

type id =  string loc (* Identifiers *)

type unop =
| Neg
| Lognot
| Bitnot

type binop =
| Add
| Sub
| Mul
| Eq
| Neq
| Lt
| Lte
| Gt
| Gte
| And
| Or
| IAnd
| IOr
| Shl
| Shr
| Sar

type _cond = CBool of bool 
and cond = _cond loc

type _exp =
| CInt
| CStr of string
| CArr of exp list
| NewArr of exp
| Id of id
| Index of exp * exp 
| Call of exp * exp
| Bop of binop * exp * exp 
| Uop of unop * exp
| Paren of exp
and exp = _exp loc

type cfield = id * exp 

type _vdecl = { id: id; init: exp;}
and vdecl = _vdecl loc

type _stmt =
| CNull
| Assn of exp * exp
| Decl of vdecl
| Ret of exp option
| SCall of exp * exp
| If of cond * stmt list * stmt list
| For of vdecl list * exp option * stmt option * stmt list
| While of exp * stmt
| Block of stmt 
and stmt = _stmt loc

type prog = stmt