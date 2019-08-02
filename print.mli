(** Provides helper functions to print on an output channel.
    Some functions use a current level of indentation which starts at 0
    and can be updated by adding an offset to it. *)

(** [nl out] prints on the output channel [out] a newline and then
    the current level of indentation. *)
val nl : out_channel -> unit

(** [indent offset producer out x] prints on the output channel [out]
    the object [x] using [producer]. The [offset] is added to the
    current level of indentation. A newline and the new level of indentation
    is printed before the object [x]. *)
val indent : int -> (out_channel -> 'a -> unit) -> out_channel -> 'a -> unit

(** [indent_t offset producer out] prints on the output channel [out]
    using [producer]. The [offset] is added to the current level of indentation.
    A newline and the new level of indentation is printed before using the [producer]. *)
val indent_t : int -> (out_channel -> unit) -> out_channel -> unit

(** [list elem out xs] prints on the output channel [out] the list [xs]
    using [elem] to print each element. *)
val list : (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit

(** [prec_list delim elem out xs] prints on the output channel [out]
    the list [xs] using [elem] to print each element. Each element of
    the list is preceded by a delimiter which is printed with [delim]. *)
val prec_list : (out_channel -> unit) -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit

(** [term_list delim elem out xs] prints on the output channel [out]
    the list [xs] using [elem] to print each element. Each element of
    the list is terminated by a delimiter which is printed with [delim]. *)
val term_list : (out_channel -> unit) -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit

(** [sep_list delim elem out xs] prints on the output channel [out]
    the list [xs] using [elem] to print each element. Each element of
    the list is separated by a delimiter which is printed with [delim]. *)
val sep_list : (out_channel -> unit) -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit

(** [space out] prints a whitespace on the output channel [out]. *)
val space : out_channel -> unit

(** [comma out] prints a comma on the output channel [out]. *)
val comma : out_channel -> unit

(** [semicolon out] prints a semicolon on the output channel [out]. *)
val semicolon : out_channel -> unit

(** [print_string out s] prints the string [s] on the output channel [out]. *)
val print_string : out_channel -> string -> unit
