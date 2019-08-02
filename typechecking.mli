(** Typechecks the abstract syntax tree of the MiniJava program and raise
    an [Error] exception if a typechecking error occurs. *)

(** This [Error] exception is raised when a typechecking error occurs. *)
exception Error of string

(** [typecheck_program prog] typechecks the abstract syntax tree [prog] and raises an [Error] exception if something is wrong. *)
val typecheck_program : LMJ.program -> unit
