type ty = TyString of string 

type id = IdString of string

type expr = Call of id

type param = Param of ty 

type t =
| Decl1 of ty * id * expr
| Decl2 of ty * id * param

