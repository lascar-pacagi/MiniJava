(** Wraps a content with position informations. We use this module to
    associate location within the source file to identifers and expressions. *)

type 'a t

(** [make startpos endpos content] create a location from a starting position [startpos]
    and an ending position [endpos] obtained from the lexbuf and a [content] (an identifier or an expression in our case).
    We use this to keep track of position informations for error messages. *)
val make : Lexing.position -> Lexing.position -> 'a -> 'a t

(** [content loc] gets the content of location [loc]. *)
val content : 'a t -> 'a

(** [startpos loc] gets the starting position of location [loc]. *)
val startpos : 'a t -> Lexing.position

(** [endpos loc] gets the ending position of location [loc]. *)
val endpos : 'a t -> Lexing.position
