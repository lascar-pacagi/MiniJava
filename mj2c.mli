(** Transpiles the abstract syntax tree of MiniJava to C. *)

(** [program2c out prog] transpiles the abstract syntax tree [prog] of the MiniJava program
    to C and prints it on the [out] channel. *)
val program2c : out_channel -> MJ.program -> unit
