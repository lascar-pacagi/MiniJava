(** Transforms the abstract syntax tree of a MiniJava program back into a MiniJava source file. *)

(** [print_program out prog] transforms the abstract syntax tree [prog] back into Java and write the result on the [out] channel.
   We can use this to check the result of parsing. We can use this too to indent a program. *)
val print_program : out_channel -> MJ.program -> unit
