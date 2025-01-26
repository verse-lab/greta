type pos = int * int    (* Line number and column *)
type t = string * pos * pos

let line_of_pos (l,_) = l
let col_of_pos (_,c) = c
let mk_pos line col = (line, col)

let file_of_range (f,_,_) = f
let start_of_range (_,s,_) = s
let end_of_range (_,_,e) = e
let mk_range f s e = (f,s,e)
let valid_pos (l,c) = l >= 0 && c >=0

let merge_range ((f,s1,e1) as r1) ((f',s2,e2) as r2) =
  if f <> f' then 
    (failwith (Printf.sprintf "merge_range called on different files: %s and %s" f f'))
  else
  if (not (valid_pos s1)) then r2 else
  if (not (valid_pos s2)) then r1 else
  mk_range f (min s1 s2) (max e1 e2)

let string_of_range ((f,(sl,sc),(el,ec))) =
  Printf.sprintf "%s:[%d.%d-%d.%d]" f sl sc el ec

let norange = ("__no_file", (0,0), (0,0))