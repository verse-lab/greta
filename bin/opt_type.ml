(* Opt_type *)

type intersect_mode = 
  | Default
  | Wo_opt1   (* Without reachability analysis *)
  | Wo_opt2   (* Without removal of duplicates *)
  | Wo_opt3   (* Without epsilon introduction  *)
  | Wo_opt123 (* Without any optimizations     *)

let intersect_mode_of_string: string -> intersect_mode = function 
  | "" | "default" -> Default
  | "wo_opt1" -> Wo_opt1
  | "wo_opt2" -> Wo_opt2
  | "wo_opt3" -> Wo_opt3
  | "wo_opt123" -> Wo_opt123
  | s -> failwith ("Unknown INTERSECT_MODE: " ^ s)


let get_intersect_mode () =
  match Sys.getenv_opt "INTERSECT_MODE" with
  | None -> Default
  | Some s -> intersect_mode_of_string (String.lowercase_ascii s)

