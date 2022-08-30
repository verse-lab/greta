(* This file is to convert CFG to productions defined in .mly and vice versa *)

let read_line i = try Some (input_line i) with End_of_file -> None

let lines_from_file (filename : string): string list =
  let rec loop inp acc =
    match (read_line inp) with
    | None -> List.rev acc
    | Some s -> 
      (* only takes lines that are relevant for productions *)
      loop inp (s :: acc) in 
    loop (open_in filename) []


(* convert_to_cfg : take mly as input and convert to cfg *)
let mly_to_cfg () = 
  Printf.printf "%s\n" "testing.."


(* convert_to_mly : take cfg as input and convert to mly *)
let cfg_to_mly () =
  Printf.printf "%s\n" "in progress.."



