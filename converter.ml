(* This file is to convert CFG to productions defined in .mly and vice versa *)

let nonterms_temp : string list ref = ref []
let terms_temp : string list ref = ref []
let starts_temp : string list ref = ref []
let prods_temp : (string * string list) list ref = ref []
let relev_lines : string list ref = ref []

let lines_from_file (filename : string): string list =
  let open String in
  let flag = ref false in
  let read_line i = 
    try Some (input_line i) with End_of_file -> None in
  let rec loop inp acc =
    match (read_line inp) with
    | None -> 
      (relev_lines := List.rev !relev_lines; List.rev acc)
    | Some s -> 
      (* only takes lines that are relevant for productions *)
      if (starts_with ~prefix:"program" s) 
        then (starts_temp := s::!starts_temp; flag := true);
      if (!flag) then (relev_lines := (s::!relev_lines); loop inp (s::acc)) 
      else loop inp acc in 
    loop (open_in filename) []

(* convert_to_cfg : take mly as input and convert to cfg *)
let mly_to_cfg () = 
  let open List in 
  let open Printf in
  lines_from_file "parser.mly" |> iter (printf "%s\n"); printf "\n";
  printf "%s\n" "Set of start symbols";
  !starts_temp |> iter (printf "\t%s\n"); printf "\n";
  printf "%s\n" "Set of productions (in progress)";
  !relev_lines |> iter (printf "\t%s\n"); printf "\n"


(* convert_to_mly : take cfg as input and convert to mly *)
let cfg_to_mly () =
  Printf.printf "%s\n" "in progress.."



