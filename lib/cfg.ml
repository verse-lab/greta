type nonterminal = string
type terminal = string
type sigma = Term of terminal | Nt of nonterminal

type production = 
  nonterminal             (* lhs *)
  * sigma list            (* full ordered rhs - set of terminals and nonterminals *)

type cfg = (* CFG := (V, \Sigma, S, P) *)
  { mutable nonterms : nonterminal list;      (* V - a set of nonterminals/variables, eg, E, +  *)
    mutable terms : terminal list;            (* \Sigma - a set of terminals, eg, N             *)
    mutable starts : nonterminal list;        (* S - start symbol, \in V, eg, E                 *)
    mutable productions : production list;    (* P - a set of productions, eg, E -> E + E       *)
  }

let null_cfg = { nonterms = []; terms = []; starts = []; productions = [] }

let sigmas_equal (a: sigma) (b: sigma): bool = 
  match a, b with 
  | Term _, Nt _ | Nt _, Term _ -> false 
  | Term a', Term b' -> (String.equal a' b')
  | Nt a', Nt b' -> (String.equal a' b')

let is_terminal (x: sigma): bool = 
  match x with Term _ -> true | Nt _ -> false

let terminal_of (x: sigma): terminal = 
  match x with Term x -> x | Nt _ -> raise (Failure "terminal_of : No Nt possible")

let first_terminal_of (sls: sigma list): string = 
  let term_ls = sls |> List.filter (fun x -> is_terminal x) in
  if List.is_empty term_ls then "" 
  else term_ls |> List.hd |> terminal_of

let production_of_id (i: int) (prods_map: (int * production) list): production = 
  match (List.assoc_opt i prods_map) with Some p -> p
  | None -> raise (Failure "production_of_id : no corresponding production")

let id_of_production (p: production) (prods_map: (int * production) list): int = 
  let rev_prods_map = 
    prods_map |> List.map (fun x -> (snd x), (fst x)) 
  in
    match (List.assoc_opt p rev_prods_map) with Some i -> i
    | None -> raise (Failure "id_of_production : no corresponding id")

let id_of_prod_for_tree_gen (sigls: sigma list) (prods_map: (int * production) list): int = 
  let prods_map_alt: (sigma list * int) list = 
    prods_map |> List.map (fun (i, p) -> 
      ((snd p), i))
  in 
    match (List.assoc_opt sigls prods_map_alt) with Some i -> i 
    | None -> raise (Failure "id_of_prod_for_tree_gen : no corresponding id")

let is_all_caps s =
  s = String.uppercase_ascii s

let string_to_sigma (s: string): sigma = 
  if (is_all_caps s) then Term s else Nt s

let string_ls_to_sigma_ls (sls: string list): sigma list = 
  sls |> List.map string_to_sigma