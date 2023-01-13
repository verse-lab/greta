

let gen_examples (filename: string): unit = 
  (** read lines from parser.conflicts *)
  let _ (* conflict_lines *) : string list =
    let ic = open_in filename in
    let try_read () = 
      try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = 
      match try_read () with
      | Some s -> loop (s :: acc)
      | None -> close_in ic; List.rev acc
    in loop [] 
  in
  (* To continue from here ...  *)
  Format.print_string filename


