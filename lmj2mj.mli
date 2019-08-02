(** Transforms an abstract syntax tree with position informations to an
    abstract syntax tree without position informations. *)

(** [translate_program p] gets rid of position informations in [p]. *)
val translate_program : LMJ.program -> MJ.program
