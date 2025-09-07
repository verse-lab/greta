type t =
| Decl1 of ty * id * exp
| Decl2 of ty * id * param
and ty = TyString of string
and id = IdString of string
and exp = Call of id
and param = Param of ty

