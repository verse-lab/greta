(* mly.ml - Parser for .mly grammar files *)

(* Re-export types for backward compatibility *)
include Mly_types

(* Use the new Menhir-based parser *)
let parse_mly_file = Mly_parse.parse_mly_file

(* Pretty print the results *)
let print_item = function
  | T { name; binding = None } -> Printf.sprintf "T(%s)" name
  | T { name; binding = Some b } -> Printf.sprintf "T(%s=%s)" b name
  | NT { name; binding = None } -> Printf.sprintf "NT(%s)" name
  | NT { name; binding = Some b } -> Printf.sprintf "NT(%s=%s)" b name

let print_results result =
  Printf.printf "=== PREAMBLE CODE ===\n%s\n\n" result.preamble.code;

  Printf.printf "=== ANNOTATIONS ===\n";
  List.iter (fun annot ->
    Printf.printf "%s %s\n" annot.prefix annot.state
  ) result.preamble.annotations;
  Printf.printf "\n";

  Printf.printf "=== PRODUCTIONS ===\n";
  List.iter (fun prod ->
    if prod.rhs = [] then
      Printf.printf "%s -> ε\n" prod.lhs
    else
      Printf.printf "%s -> %s\n" prod.lhs
        (String.concat " " (List.map print_item prod.rhs));
    if prod.action <> "" then
      Printf.printf "  Action: %s\n" prod.action;
    Printf.printf "\n"
  ) result.productions

let separator = "%%"

let mly_of_ta (ta: Ta.ta) (mly: parsed_mly) (mly_production_of_symbol: Ta.symbol -> production): string =
  let transitions = Hashtbl.to_seq ta.Ta.transitions in

  (* Group transitions by state *)
  let state_transitions: (Ta.state, (Ta.symbol * Ta.beta list) list) Hashtbl.t = Hashtbl.create 10 in
  Seq.iter (fun ((state, symbol), rhs) ->
    let existing = match Hashtbl.find_opt state_transitions state with
    | Some lst -> lst
    | None -> []
    in
    Hashtbl.replace state_transitions state ((symbol, rhs) :: existing)
  ) transitions;
  let state_transitions = (Hashtbl.to_seq state_transitions)
    |> List.of_seq
    |> List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) in

  (* old to new state map *)
  let state_map: (string, string) Hashtbl.t = Hashtbl.create 10 in

  (* Build the mly string *)
  let postfix = List.fold_left (fun acc (state, transitions) ->
    let state_str = Printf.sprintf "%s:\n" state in
    let trans_str = List.fold_left (fun t_acc (symbol, rhs) ->
      let mly_prod = mly_production_of_symbol symbol in
      Hashtbl.add state_map mly_prod.lhs state;
      let mly_rhs = mly_prod.rhs in
      let action = mly_prod.action in
      (assert (List.length rhs = List.length mly_rhs));
      let rhs_zip = List.combine rhs mly_rhs in
      let rhs_str = List.fold_left (fun s_acc (b, item) ->
        match b, item with
        | Ta.T t, T _ when t = Converter.empty_term -> s_acc ^ " "
        | Ta.T t, T { name = _; binding = None} -> s_acc ^ t ^ " "
        | Ta.T t, T { name = _; binding = Some b } -> s_acc ^ (b ^ "=" ^ t) ^ " "
        | Ta.S s, NT { name = _; binding = None } -> s_acc ^ s ^ " "
        | Ta.S s, NT { name = _; binding = Some b } -> s_acc ^ (b ^ "=" ^ s) ^ " "
        | _, _ -> raise (Failure "mly_of_ta: Mismatched beta and item")
      ) "" rhs_zip in
      t_acc ^ Printf.sprintf "  | %s %s\n" rhs_str action
    ) "" transitions in
    acc ^ "\n" ^ state_str ^ trans_str ^ "  ;\n"
  ) (separator ^ "\n") state_transitions in

  mly.preamble.code
  ^
  (
    mly.preamble.annotations
    |> List.fold_left (fun acc annot ->
        acc ^ Printf.sprintf "%s %s\n" annot.prefix (match Hashtbl.find_opt state_map annot.state with
          | Some s -> s
          | None ->
            Printf.eprintf "Warning: Could not find state for annotation %s -- %s\n" annot.prefix annot.state;
            exit 1
        )
      ) ""
  )
  ^ postfix
