module C = Cfg

type state = string
type symbol = string * int
type transition = state * (symbol * state list)

type ta = (* TA := (Q, F, Q_f, \Del) *)
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable final_state : state;
    mutable transitions : transition list;
  }

let cfg_to_ta (c: C.cfg): ta =
  let ta_res: ta = { states = c.nonterms; alphabet = []; final_state = c.start; transitions = [] } in
  let debug_print = ref true in
  let open List in
  let open Printf in
  let rank_of_symb (s: string): int =
    let prods_rhs = c.prods |> map snd in 
    if (s = "N" || s = "B") then 0 else
    match assoc_opt s prods_rhs with None -> raise (Failure "Infeasible: nonexisting symbol")
    | Some symb_ls -> length symb_ls
  in let ranked_alphabet: symbol list = c.terms |> filter (fun x -> not (x = "THEN") && not (x = "ELSE") && not (x = "RPAREN"))
    |> map (fun x -> if (x = "LPAREN") then "LPARENRPAREN" else x) |> map (fun x -> (x, rank_of_symb x))
  in ta_res.alphabet <- ranked_alphabet;
  if (!debug_print) then (printf "\nTA obtained from the original CFG : \n";
  printf "\tStates : { "; ta_res.states |> iter (printf "%s "); printf "}\n";
  printf "\tAlphabet : { "; ta_res.alphabet |> iter (fun x -> printf " <%s, %d> " (fst x) (snd x) ); printf "}\n"
  );
  ta_res


