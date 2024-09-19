(*  Ast.ml 
 * 
 *  A simple example of an abstract syntax tree definition.
 *  This ast of "bexp" represents a grammar for boolean 
 *  formula expressions with 'and', 'or', 'not', and implication.
 *)

(* boolean expressions *)
type bexp =
  | Var of string
  | True
  | False
  | And of bexp * bexp
  | Or  of bexp * bexp
  | Imp of bexp * bexp
  | Not of bexp


(* defines the precedence of each operator; useful for
 * pretty printing 
 *)
let prec_of_bexp (b:bexp) : int =
  begin match b with
    | Imp _ -> 10
    | Or  _ -> 20
    | And _ -> 30
    | Not _ -> 40
    | Var _ | True | False -> 50
  end

let full_parens = ref false

(* When printing an abstract syntax tree, it is usually 
 * necessary to re-introduce parentheses that were eliminated
 * during the parsing process.  
 *
 * One could parenthesize every subexpression, but that 
 * prints way too many parens.
 *
 * Instead, use the precedence level l of the outer expression
 * relative to the precedence level (prec) of the inner 
 * expression to determine whether parentheses are needed.
 *
 * If the inner precedence is lower than the outer precedence,
 * then you need to add parentheses around the inner expression.
 *)
let string_of_bexp (b:bexp) : string =
  (* b is the bexp, l is the outer precedence level *)
  let rec sob b l =
    let prec = prec_of_bexp b in 
      (if (prec < l) || !full_parens then "(" else "") ^
	begin match b with
	  | Var s -> s
	  | True -> "true"
	  | False -> "false"
	  | And(b1, b2) -> (sob b1 prec) ^ " & " ^ (sob b2 prec)
	  | Or (b1, b2) -> (sob b1 prec) ^ " | " ^ (sob b2 prec)
	  | Imp(b1, b2) -> (sob b1 15)  ^ " -> " ^ (sob b2 prec)
	  | Not(b1)     -> "~" ^ (sob b1 prec)
	end ^
	(if (prec < l) || !full_parens then ")" else "") 
  in
    sob b 0