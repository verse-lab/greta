open Graphics

(** utilities for drawing *)
let pastel_yellow = rgb 255 255 153
let pastel_purple = rgb 150 120 250
let pastel_green = rgb 204 255 204
let dark_orange = rgb 255 153 0
let light_green = rgb 204 255 204
let dark_navy = rgb 0 21 51

(** clear window and set background color *)
let set_background color = 
  let fg = foreground 
  in
      set_color color;
      fill_rect 0 0 (size_x ()) (size_y ());
      set_color fg



