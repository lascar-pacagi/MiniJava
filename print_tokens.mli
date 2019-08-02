(** Prints the tokens of the MiniJava source file on an output channel. *)

(** [print out lexbuf show_loc] prints the tokens obtained from [lexbuf] on the [out] channel.
    If [show_loc] is true, prints position informations too. *)
val print: out_channel -> Lexing.lexbuf -> bool -> unit