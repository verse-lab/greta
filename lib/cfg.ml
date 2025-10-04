type nonterminal = string
type terminal = string
type sigma = T of terminal | Nt of nonterminal
type production = 
  nonterminal (* lhs *)
  * (terminal * nonterminal list) (* rhs with identifying symbol, and nonterminals *)

type production = 
  nonterminal (* lhs *)
  * (terminal * int) (* symbol = (first occurring terminal, rank) *)
  * sigma list (* full ordered rhs - set of terminals and nonterminals *)

type cfg = (* CFG := (V, \Sigma, S, P) *)
  { mutable nonterms : nonterminal list;      (* V - a set of nonterminals/variables, eg, E, +  *)
    mutable terms : terminal list;            (* \Sigma - a set of terminals, eg, N             *)
    mutable starts : nonterminal list;        (* S - start symbol, \in V, eg, E                 *)
    mutable productions : production list;    (* P - a set of productions, eg, E -> E + E       *)
  }

let null_cfg = { nonterms = []; terms = []; starts = []; productions = [] }

let sigmas_equal (a: sigma) (b: sigma): bool = 
  match a, b with 
  | T _, Nt _ | Nt _, T _ -> false 
  | T a', T b' -> (String.equal a' b')
  | Nt a', Nt b' -> (String.equal a' b')

let is_terminal (x: sigma): bool = 
  match x with T _ -> true | Nt _ -> false
