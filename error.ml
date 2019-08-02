open Lexing

let position pos =
  (** [pos.pos_fname] is the name of the file. *)
  let file = pos.pos_fname in
  (** [pos.pos_lnum] is the current line number (starting from 1). *)
  let l = pos.pos_lnum in
  (** [pos.pos_cnum] is the current character number from the beginning
      of the file (starting from 0).
      [pos.pos_bol] is the current character number from the beginning
      of the file to the beginning of the current line.
   *)
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "file \"%s\", line %d, character %d" file l c

let positions pos1 pos2 =
  let file = pos1.pos_fname in
  let line = pos1.pos_lnum in
  let char1 = pos1.pos_cnum - pos1.pos_bol + 1 in
  let char2 = pos2.pos_cnum - pos1.pos_bol + 1 in
  Printf.sprintf "File \"%s\", line %d, characters %d-%d" file line char1 char2
