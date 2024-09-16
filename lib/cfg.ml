type nonterminal = string
type terminal = string
type production = nonterminal * (terminal * string list)

type cfg = (* CFG := (V, \Sigma, S, P) *)
  { mutable nonterms : nonterminal list;   (* V - a set of nonterminals/variables, eg, E, +  *)
    mutable terms : terminal list;         (* \Sigma - a set of terminals, eg, N             *)
    mutable start : nonterminal;           (* S - start symbol, \in V, eg, E                 *)
    mutable productions : production list; (* P - a set of productions, eg, E -> E + E       *)
  }

let null_cfg = { nonterms = []; terms = []; start = ""; productions = [] }

type nt = string
type t = string
type sigma = T of t | Nt of nt
type p = nt * int * sigma list

type cfg2 = {
  mutable nonterms : nt list;
  mutable terms : t list;
  mutable start : nt;
  mutable productions : p list;
}

let null_cfg2 = { nonterms = []; terms = []; start = ""; productions = [] }
