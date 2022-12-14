module C = Cfg

type state = string
type symbol = string * int
type transition = state * (symbol * state list)

type ta = (* TA := (Q, F, Q_s, \Del) *)
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable start_state : state;
    mutable transitions : transition list;
  }

let cfg_to_ta (c: C.cfg): ta =
  let open List in
  let open Printf in
  let debug_print = ref true in
  (** helpers **)
  let rank_of_symb (s: string): int =
    let prods_rhs = c.prods |> map snd in 
    if (s = "N" || s = "B") then (printf "Symbol N or S, so length 0.\n"; 0) else
    match assoc_opt s prods_rhs with 
    | None -> raise (Failure "Infeasible: nonexisting symbol")
    | Some symb_ls -> (printf "Symbol %s has" s; symb_ls |> iter (printf " %s"); printf " so length is " ; 
      printf "%d.\n" (length symb_ls); length symb_ls) in
  let ranked_alphabet: symbol list = 
    let rparen_exists = mem "RPAREN" c.terms in c.terms 
    |> filter (fun x -> not (x = "THEN") && not (x = "ELSE") && not (x = "RPAREN"))
    |> map (fun x -> if (x = "LPAREN" && rparen_exists) then "LPARENRPAREN" else x) |> map (fun x -> (x, rank_of_symb x))
  and trans: transition list = 
    c.prods |> fold_left (fun acc (n, (t, ls)) -> (n, ((t, rank_of_symb t), ls)) :: acc) []
    |> map (fun (s, (op, s_ls)) -> if (fst op = "ε" && length s_ls = 1) then (s, ((hd s_ls, 1), "ϵ"::[])) else (s, (op, s_ls))) in
  let ta_res: ta = { states = "ϵ"::c.nonterms; alphabet = ranked_alphabet; start_state = c.start; transitions = List.rev trans } in
  if (!debug_print) then (printf "\nTA obtained from the original CFG : \n";
  printf "\tStates : { "; ta_res.states |> iter (printf "%s "); printf "}\n";
  printf "\tAlphabet : { "; ta_res.alphabet |> iter (fun x -> printf " <%s, %d> " (fst x) (snd x) ); printf "}\n";
  printf "\tFinal State : { %s }\n" ta_res.start_state;
  printf "\tTransitions : { \n"; ta_res.transitions |> iter (fun x -> printf "\t\t\t%s ->_{%s} " 
  (fst x) (fst (fst (snd x))); (snd (snd x)) |> iter (printf "%s "); printf "\n"); printf "\t\t      }\n");
  ta_res


  

