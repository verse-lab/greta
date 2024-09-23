type ast =
  | Atom of string
  | A1 of ast
  | B1 of ast
  | A2 of ast * ast
  | B2 of ast * ast

