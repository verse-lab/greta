type ident = string

(* Table and column names *)
type table = Table of ident
type col   = Col of ident

(* Expressions (used for values and in simple comparisons) *)
type expr =
  | Var   of string
  | Str   of string
  | Int   of int
  | Float of float
  | Plus  of expr * expr
  | Minus of expr * expr
  | Star  of expr * expr
  | Div   of expr * expr

(* Values in assignments *)
type value =
  | ValueExpr of expr
  | Default

(* Column assignment: col = value *)
type assignment = Assign of col * value

(* WHERE conditions *)
type cond =
  | And       of cond * cond
  | Or        of cond * cond
  | CondAssign of assignment                          (* e.g., col = expr inside WHERE *)
  | Compare   of string * col * expr                  (* token COMPARISION_OPERATOR as raw string *)
  | Between   of col * expr * expr

(* ORDER BY *)
type order_dir = [ `Asc | `Desc ]
type order = Order of col * order_dir
type order_by_clause = OrderBy of order list

(* WHERE / LIMIT *)
type where_clause = Where of cond list
type limit_clause = Limit of int

(* Top-level statement (program produces a single UPDATE in your grammar) *)
type t =
  | Update of table
           * assignment list
           * where_clause option
           * order_by_clause option
           * limit_clause option

(* For compatibility with any %type you declared but don't use yet *)
type exp = expr
type constr = unit
