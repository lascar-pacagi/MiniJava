open Printf

let maxindent =
  120

(** [whitespace] is a string of [maxindent] whitespaces. *)
let whitespace =
  String.make maxindent ' '

(** [indentation] is the current level of indentation. *)
let indentation =
  ref 0

let nl out =
  fprintf out "\n%s" (String.sub whitespace 0 !indentation)

let indent
      (offset : int)
      (producer : out_channel -> 'a -> unit)
      (out : out_channel)
      (x : 'a) : unit =
  let old_indentation = !indentation in
  let new_indentation = old_indentation + offset in
  if new_indentation <= maxindent then
    indentation := new_indentation;
  fprintf out "%t%a" nl producer x;
  indentation := old_indentation

let indent_t
      (offset : int)
      (producer : out_channel -> unit)
      (out : out_channel) : unit =
  let old_indentation = !indentation in
  let new_indentation = old_indentation + offset in
  if new_indentation <= maxindent then
    indentation := new_indentation;
  fprintf out "%t%t" nl producer;
  indentation := old_indentation

let rec list
          (elem : out_channel -> 'a -> unit)
          (out : out_channel)
          (xs : 'a list) : unit =
  match xs with
  | [] ->
      ()
  | x :: xs ->
      fprintf out "%a%a" elem x (list elem) xs

let rec prec_list
          (delim : out_channel -> unit)
          (elem : out_channel -> 'a -> unit)
          (out : out_channel)
          (xs : 'a list) : unit =
  match xs with
  | [] ->
      ()
  | x :: xs ->
      fprintf out "%t%a%a" delim elem x (prec_list delim elem) xs

let rec term_list delim elem out xs =
  match xs with
  | [] ->
    ()
  | x :: xs ->
      fprintf out "%a%t%a" elem x delim (term_list delim elem) xs

let sep_list sep elem out xs =
  match xs with
  | [] ->
      ()
  | x :: xs ->
      fprintf out "%a%a" elem x (prec_list sep elem) xs

let space out =
  fprintf out " "

let comma out =
  fprintf out ", "

let semicolon out =
  fprintf out ";"

let print_string out s =
  fprintf out "%s" s
