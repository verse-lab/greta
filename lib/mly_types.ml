(* mly_types.ml - Shared types for .mly parsing *)

type binding = {
  name: string;
  binding: string option;
}

type item =
  | NT of binding
  | T of binding

type production = {
  lhs: string;
  rhs: item list;
  action: string;
}

type annotation = {
  prefix: string;
  state: string;
}

type preamble = {
  code: string;
  annotations: annotation list;
}

type parsed_mly = {
  preamble: preamble;
  productions: production list;
}
