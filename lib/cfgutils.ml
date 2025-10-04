module G = Cfg
module A = Ta

let wrapped_printf debug fmt =
    if debug then Printf.printf fmt
    else Printf.ifprintf stdout fmt

(* *** Newly added helpers for the revamped version *** *)

let sigma_to_beta (s: G.sigma): A.beta = 
  match s with 
  | Term t -> T t
  | Nt n -> S n

let sigma_list_to_beta_list (sigls: G.sigma list): A.beta list = 
  sigls |> List.map sigma_to_beta  

let prods_starting_from_nonterm (nonterm: G.nonterminal) (prods: G.production list): 
  G.production list = 
  prods |> List.filter (fun (nt, _sigls) -> (nt = nonterm))

(* nonterms_of_sigls : collect nonterminals from sigma list *)
let rec nonterms_of_sigls (sigls: G.sigma list) (acc: G.nonterminal list): G.nonterminal list = 
  match sigls with [] -> List.rev acc
  | sig_hd :: sig_tl -> 
    (match sig_hd with 
    | Term _ -> nonterms_of_sigls sig_tl acc 
    | Nt nt -> nonterms_of_sigls sig_tl (nt::acc))

(* rhs_nonterms_of_prods : collect nonterminals that  is different from lhs nonterminal *)
let rhs_nonterms_of_prods (lhs_nonterm: G.nonterminal) (prods: G.production list) = 
  let rec loop (lhs_nt: G.nonterminal) (ls: G.production list) (acc: G.nonterminal list) = 
    match ls with 
    | [] -> acc |> List.filter (fun x -> not (x = lhs_nt)) 
    | (nt, sigls) :: tl -> 
      let to_acc = nonterms_of_sigls sigls [] in
      loop nt tl (acc@to_acc) 
  in loop lhs_nonterm prods []

let lhs_nonterm_of_prods (prods: G.production list): G.nonterminal = 
  if (List.is_empty prods) then raise (Failure "lhs_nonterm_of_prods : empty productions") 
  else prods |> List.hd |> fst

let remove_dups ls =
  let unique_cons elem ls = if (List.mem elem ls) then ls else elem :: ls in
    List.fold_right unique_cons ls []

let group_productions (nonterms: G.nonterminal list) (prods: G.production list) (debug: bool): 
  (G.nonterminal * G.production list) list = 
  let prods_grouped = nonterms
    |> List.map (fun nt -> nt, (prods_starting_from_nonterm nt prods))
  in if debug then (wrapped_printf debug "\n\t* Prods grouped:\n"; 
  prods_grouped |> List.iter (fun (nt, prods) -> wrapped_printf debug "\tLHS nonterm: %s\n" nt; 
  Pp.pp_productions prods));prods_grouped

let sigma_of_beta (b: A.beta): G.sigma = 
  match b with A.T t ->  G.Term t | A.S s -> G.Nt s

let production_of_beta_list (bls: A.beta list): G.sigma list = 
  bls |> List.map sigma_of_beta
  