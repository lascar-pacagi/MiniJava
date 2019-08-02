type 'a t = {
  startpos: Lexing.position; (** The starting position of the content within the file. *)
  endpos: Lexing.position; (** The ending position of the content within the file. *)
  content: 'a
}

let make startpos endpos content = {
  startpos = startpos;
  endpos = endpos;
  content = content
}

let content { content = c } = c

let startpos { startpos = pos } = pos

let endpos { endpos = pos } = pos
