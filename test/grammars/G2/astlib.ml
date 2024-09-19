(* astlib.ml *)

(* Helper functions of abstract syntax of trees. *)
(******************************************************************************)

open Format
open Ast
open Range  

(* 
 * Parse an AST from a lexbuf 
 * - the filename is used to generate error messages
 *)
(*  
let parse (filename : string) (buf : Lexing.lexbuf) : Ast.prog =
  try
    Lexer.reset_lexbuf filename 1 buf;
    Parser.toplevel Lexer.token buf
  with Parsing.Parse_error ->
    failwith (Printf.sprintf "Parse error at %s." (Range.string_of_range (Lexer.lex_range buf)))
*)


(** Precedence of binary operators. Higher precedences bind more tightly. *)
let prec_of_binop = function
| Mul -> 100
| Add | Sub -> 90

(** Precedence of expression nodes. *)
let prec_of_exp = function
| Bop (o,_,_) -> prec_of_binop o
| _ -> 200

let string_of_binop = function
| Mul   -> "*"
| Add   -> "+"
| Sub   -> "-"

let print_id_aux fmt t =
  pp_print_string fmt t.elt

let rec print_list_aux fmt sep pp l =
  begin match l with
    | [] -> ()
    | h::[] -> pp fmt h
    | h::tl -> 
	pp fmt h;
	sep ();
	print_list_aux fmt sep pp tl
  end

let rec print_const_aux fmt c =
  begin match c.elt with
    | CInt  v -> pp_print_string fmt (Int64.to_string v)
  end

and print_exp_aux fmt level e =
  let pps = pp_print_string fmt in
  let this_level = prec_of_exp e.elt in
  (if this_level < level then pps "(");
  (match e.elt with
  | Const  c -> print_const_aux fmt c
  | Id i -> print_id_aux fmt i
  | Bop (o,l,r) ->
      pp_open_box fmt 0;
      print_exp_aux fmt this_level l;
      pp_print_space fmt ();
      pp_print_string fmt (string_of_binop o);
      pp_print_space fmt ();
      print_exp_aux fmt this_level r;
      pp_close_box fmt ()
  );
  (if this_level < level then pps ")" )

let print_decl_aux semi fmt {elt={id; init}} =
  pp_open_hbox fmt ();
  pp_print_string fmt "int";
  pp_print_space fmt ();
  print_id_aux fmt id;
  pp_print_space fmt ();
  pp_print_string fmt " =";
  pp_print_space fmt ();
  print_exp_aux fmt 0 init;
  pp_print_string fmt semi;
  pp_close_box fmt ()

let rec print_block_aux fmt stmts =
  let pps = pp_print_string fmt in
  if (List.length stmts) > 0 then begin
    pps "{"; pp_force_newline fmt ();
    pps "  "; pp_open_vbox fmt 0;
    print_list_aux fmt (fun () -> pp_print_space fmt ()) print_stmt_aux stmts;
    pp_close_box fmt (); pp_force_newline fmt ();
    pps "}"
  end else pps "{ }"

and print_cond_aux fmt b_then opt_b_else =
  let pps = pp_print_string fmt in
  print_block_aux fmt b_then;
  begin match opt_b_else with
    | [] -> ()
    | b_else ->
      pps " else ";
      print_block_aux fmt b_else
  end

and print_stmt_aux fmt s =
  let pps = pp_print_string fmt in
  let ppsp = pp_print_space fmt in
  begin match s.elt with
    | Decl d -> print_decl_aux ";" fmt d

    | Assn (i,e) ->
	pp_open_box fmt 0;
	print_id_aux fmt i;
	pps " =";
	ppsp ();
	print_exp_aux fmt 0 e;
	pps ";";
	pp_close_box fmt ()

    | Ret (e) -> 
      pps "return";
      pps " "; print_exp_aux fmt 0 e; pps ";"

    | If(e, b_then, opt_b_else) ->
      pps "if ("; print_exp_aux fmt 0 e; pps ") ";
      print_cond_aux fmt b_then opt_b_else
        
    | While(e, b) ->
	pps "while ("; print_exp_aux fmt 0 e; pps ") ";
	print_block_aux fmt b

    | Block(b) -> print_block_aux fmt b
  end

let print_prog_aux fmt p =
  pp_open_vbox fmt 0;
  List.iter (fun s ->
      print_stmt_aux fmt s;
      pp_force_newline fmt ()
    ) p;
  pp_close_box fmt ()

let print_prog (p:prog) : unit =
  pp_open_hvbox std_formatter 0;
  print_prog_aux std_formatter p;
  pp_close_box std_formatter ();
  pp_print_newline std_formatter ()

let string_of_prog (p:prog) : string =
  pp_open_hvbox str_formatter 0;
  print_prog_aux str_formatter p;
  pp_close_box str_formatter ();
  flush_str_formatter ()

let print_stmt (s:stmt) : unit =
  pp_open_hvbox std_formatter 0;
  print_stmt_aux std_formatter s;
  pp_close_box std_formatter ();
  pp_print_newline std_formatter ()

let string_of_stmt (s:stmt) : string =
  pp_open_hvbox str_formatter 0;
  print_stmt_aux str_formatter s;
  pp_close_box str_formatter ();
  flush_str_formatter ()

let print_block (b:block) : unit =
  pp_open_hvbox std_formatter 0;
  print_block_aux std_formatter b;
  pp_close_box std_formatter ();
  pp_print_newline std_formatter ()
  
let string_of_block (b:block) : string =
  pp_open_hvbox str_formatter 0;
  print_block_aux str_formatter b;
  pp_close_box str_formatter ();
  flush_str_formatter ()

let print_exp (e:exp) : unit =
  pp_open_hvbox std_formatter 0;
  print_exp_aux std_formatter 0 e;
  pp_close_box std_formatter ();
  pp_print_newline std_formatter ()

let string_of_exp (e:exp) : string =
  pp_open_hvbox str_formatter 0;
  print_exp_aux str_formatter 0 e;
  pp_close_box str_formatter ();
  flush_str_formatter ()