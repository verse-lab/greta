open Ta
open Graphics
open Utils

let draw_two_branches () =
  set_color black

let draw_tree (e: tree) (id: string): unit =
  let sz_w, sz_h = 460, 900 in
  let num_branches = branches_of_tree e in
  let open Printf in
  printf "\n\n\tTree %s\n\n" id;
  open_graph (sprintf " %ix%i" (sz_w * 2) sz_h);
  set_window_title "Choose your preference";
  set_background dark_navy;

  (* root node *)
  set_color pastel_yellow;
  fill_rect 180 775 100 50;
  set_color pastel_purple;
  draw_rect 180 775 100 50;

  if (num_branches = 2) then (
    printf "Number of branches is 2"

  ) else if (num_branches = 3) then (
    printf "Number of branches is 3"

  ) else (
    printf "Number of branches is neither 2 nor 3 for symbol"

  )
  (* ;

  close_graph () *)
  
  (*
  Node ("PLUS"  [ Leaf expr ;  Leaf expr ])
  
  height: 1
  
        +       | 6 ^ sym
      /   \     | 4 ^ / ^ 3 ^ \
    expr  expr  | 2 ^ ld ^ 2 ^ ld
  
  Node ("IF"  [ Leaf cond_expr ;  Leaf expr ;  Node ("PLUS"  [ Leaf expr ;  Leaf expr ])])
  
  height: 2
  
        IF      | 6 ^ sym
      /   \     | 4 ^ / ^ 3 ^ \
    cond_expr  expr  | 2 ^ ld ^ 2 ^ ld
  
  
  
           "+"      | (4+5) ^ "sym"
          /   \     | (4+4) ^ / ^ 3 ^ \
        expr  expr  | (4+2) ^ ld ^ 2 ^ ld
  
  
  
  *)
  