(* Ranges and utilities on ranges. *)

(* A range represents a segment of text in a given file; it has a
 * beginning and ending position specified in terms of line and column
 * numbers. A range is associated with tokens during lexing to allow
 * the compiler to give better error messages during lexing and
 * parsing.  
 *)

(* a position in the source file *)
type pos

(* line of position *)
val line_of_pos : pos -> int

(* column of position *)
val col_of_pos : pos -> int

(* new position with given line and col *)
val mk_pos : int -> int -> pos

(* a range of positions in a particular file *)
type t

(* the filename a range is in *)
val file_of_range : t -> string

(* the beginning of the range *)
val start_of_range : t -> pos

(* the end of the range *)
val end_of_range : t -> pos

(* create a new range from the given filename and start, end positions *)
val mk_range : string -> pos -> pos -> t

(* merge two ranges together *)
val merge_range : t -> t -> t

(* pretty-print a range, leaving out the file *)
val string_of_range : t -> string

(* use to tag generated AST nodes where range does not apply *)
val norange : t