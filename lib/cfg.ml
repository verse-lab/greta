type nonterminal = string
type terminal = string
type sigma = T of terminal | Nt of nonterminal
type production = 
  nonterminal (* lhs *)
  * (terminal * nonterminal list) (* rhs with identifying symbol, and nonterminals *)

type production2 = 
  nonterminal (* lhs *)
  * ((terminal * int) * nonterminal list) (* rhs with identifying symbol, and nonterminals *)
  * sigma list (* full ordered rhs *)
(* symbol = (identifying symbol, rank) *)

type cfg = (* CFG := (V, \Sigma, S, P) *)
  { mutable nonterms : nonterminal list;   (* V - a set of nonterminals/variables, eg, E, +  *)
    mutable terms : terminal list;         (* \Sigma - a set of terminals, eg, N             *)
    mutable start : nonterminal;           (* S - start symbol, \in V, eg, E                 *)
    mutable productions : production list; (* P - a set of productions, eg, E -> E + E       *)
  }

type cfg3 = (* CFG := (V, \Sigma, S, P) *)
  { mutable nonterms : nonterminal list;   (* V - a set of nonterminals/variables, eg, E, +  *)
    mutable terms : terminal list;         (* \Sigma - a set of terminals, eg, N             *)
    mutable start : nonterminal;           (* S - start symbol, \in V, eg, E                 *)
    mutable productions : production2 list; (* P - a set of productions, eg, E -> E + E       *)
  }

let null_cfg = { nonterms = []; terms = []; start = ""; productions = [] }

type nt = string
type t = string
type p = nt * int * sigma list

type cfg2 = {
  mutable nonterms : nt list;
  mutable terms : t list;
  mutable start : nt;
  mutable productions : p list;
}

let null_cfg2 = { nonterms = []; terms = []; start = ""; productions = [] }

let sigmas_equal (a: sigma) (b: sigma): bool = 
  match a, b with 
  | T _, Nt _ | Nt _, T _ -> false 
  | T a', T b' -> a' = b'
  | Nt a', Nt b' -> a' = b'
