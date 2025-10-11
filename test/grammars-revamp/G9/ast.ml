type ident = string

type table = Table of ident
type col   = Col of ident

type expr =
  | Var   of string
  | Str   of string
  | Int   of int
  | Float of float
  | Plus  of expr * expr
  | Minus of expr * expr
  | Star  of expr * expr
  | Div   of expr * expr

type value =
  | ValueExpr of expr
  | Default

type assignment = Assign of col * value

type cond =
  | And       of cond * cond
  | Or        of cond * cond
  | CondAssign of assignment 
  | Compare   of string * col * expr 
  | Between   of col * expr * expr

type order_dir = [ `Asc | `Desc ]
type order = Order of col * order_dir
type order_by_clause = OrderBy of order list

type where_clause = Where of cond list
type limit_clause = Limit of int

type t =
  | Update of table
           * assignment list
           * where_clause option
           * order_by_clause option
           * limit_clause option

type exp = expr
type constr = unit
