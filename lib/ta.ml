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

let syms_equals s1 s2 = (fst s1) = (fst s2)

let null_ta = { states = []; alphabet = []; start_state = ""; transitions = [] }

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

let node_symbol (e: tree): string =
  match e with Leaf _ -> "dummy"
  | Node (s, _) -> fst s

let change_node_symbol_with (e: tree) (sym: string): tree =
  match e with Leaf v -> Leaf v
  | Node ((_, ar), subts) -> Node ((sym, ar), subts)

let branches_of_tree (e:tree): int =
  match e with Leaf _ -> 0
  | Node (_, subts) -> List.length subts

