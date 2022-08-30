(* CFG := (V, \Sigma, S, P) *)
module type CFG = sig
    type nonterms = string list
    type terms = string list
    type starts = string list
    type productions = (string * string list) list
end








