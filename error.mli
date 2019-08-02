(** Transforms position informations to string. *)

(** [position pos] returns a string from a position [pos]. The string contains the file source
   and the starting position (line number and character number from the beginning of the line). *)
val position : Lexing.position -> string

(** [position pos1 pos2] returns a string from two positions [pos1] and [pos2].
   The string contains the file source, the starting position and the ending position from that file. *)
val positions : Lexing.position -> Lexing.position -> string
