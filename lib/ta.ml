type state = string
type symbol = string * int
type transition = state * (symbol * state list)

type load = string
type tree = Leaf of load | Node of (symbol * (tree list))

(* Top-down TA := (Q, F, Q_s, \Del) *)
type ta =
  { mutable states : state list;
    mutable alphabet : symbol list;
    mutable start_state : state;
    mutable transitions : transition list;
  }

let sym_equals sym str = (fst sym = str)

(* let null_ta = { states = []; alphabet = []; start_state = ""; transitions = [] } *)

let is_leaf (t: tree): bool =
  match t with Leaf _ -> true
  | Node (_, _) -> false

(** return_state : return state (load of Leaf) *)
let return_state (t: tree): state =
  match t with Leaf v -> v
  | Node (_, _) -> "Error: Not a leaf"

let arity (sym: symbol): int = snd sym

let gen_state_list (sym: symbol) (st: state): state list = 
  List.init (arity sym) (fun _ -> st)

(** height : find the height (maximum depth) of tree *)
let height (e: tree): int =
  let max_ls ls = List.fold_left max 0 ls in 
  let rec loop t acc = match t with 
    | Leaf _ -> acc
    | Node (_, ts) -> ts
      |> List.map (fun x -> loop x (acc+1)) 
      |> max_ls
  in loop e 0

(** accept : TA starts in start_state at the root and 
    moves downward along branches of the tree, 
    associating along a run a state with each subterm inductively. 
    A tree is accepted if every branch can be gone through this way *)
(** informal def : a tree gets rejected when for every initial state,
    the tree gets stuck *)
let accept (a: ta) (e: tree) (debug_print: bool): bool =
  let open List in
  let open Printf in
  if debug_print then (printf "\nCheck if TA accepts a tree..\n";);
  (* helpers *)
  let is_single_epsilon ls: bool = 
    (length ls = 1) && (return_state (hd ls) = "ϵ") in
  let is_conditional sym = (fst sym) = "IF" in
  let gen_state_name str depth = 
    (String.capitalize_ascii str) ^ "_" ^ string_of_int depth in
  let trans = a.transitions in
  (* transform tree to corresponding transition list *)
  let rec transform t trans_acc dep: transition list =
    match t with Leaf _ -> trans_acc
    | Node (s, subts) -> 
      begin match arity s with 
      | 0 -> if (is_single_epsilon subts) 
        then transform (hd subts) (("Expr_1", (("", 0), []))::trans_acc) (dep+1)
        else (printf "0-arity symbol %s does not have valid subtrees.\n" (fst s); 
        raise (Invalid_argument "Invalid tree!"))
      | n -> 
        let state_curr = gen_state_name "Expr" dep in
        if (is_conditional s) then 
         (let trans_subts: transition list = subts |> map (fun subt -> 
            transform subt trans_acc (dep+1)) |> flatten in
          let state_ls: state list = if (n=2) then ["Cond_expr"; state_curr] 
          else if (n=3) then ["Cond_expr"; state_curr; state_curr] 
          else raise (Invalid_argument "IF takes neither 2 nor 3 args") 
          in (state_curr, (("IF", n), state_ls)) :: trans_subts @ trans_acc )
        else
         (let trans_subts = subts |> map (fun subt -> transform subt trans_acc (dep+1))
          |> flatten in
          let state_ls = gen_state_list s state_curr in
          (state_curr, (s, state_ls)) :: trans_subts @ trans_acc )
       end
  in let accepted = transform e [] 1 |> fold_left 
  (fun bool_acc tran -> mem tran trans && bool_acc) true in 
  if debug_print then printf "\n\tAccepted? %b\n\n" accepted; 
  accepted



