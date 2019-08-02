open LMJ
open Printf

let indentation = 2

(** UTF8 sequences for different symbols. *)
let lbrace = "\xe2\x9d\xb4"
let rbrace = "\xe2\x9d\xb5"
let lparen = "\xef\xbc\x88"
let rparen = "\xef\xbc\x89"
let branch = "\xe2\x94\x9c"
let branch_end = "\xe2\x94\x94"
let pipe = "\xe2\x94\x82"
let rtriangle = "\xe2\x96\xb8"
let ltriangle = "\xe2\x97\x82"

(** If [show_location] is true then we print position informations associated with
    identifiers and expressions. *)
let show_location = ref false

(** [print_position out pos] prints position informations using [pos] on the
    output channel [out] iff [show_location] is true. *)
let print_position out pos =
  if !show_location then
    let l = pos.Lexing.pos_lnum in
    let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
    fprintf out " %s line %d, char %d %s" rtriangle l c ltriangle

(** [print_identifier out loc] prints on the output channel [out] the identifier contained in [loc],
    and position informations contained in [loc] iff [show_location] is true. *)
let print_identifier out loc =
  if !show_location then
    fprintf out "%s%a" (Location.content loc) print_position (Location.startpos loc)
  else
    fprintf out "%s" (Location.content loc)

(** [print_list print prefix out l] prints the list [l] on the output channel [out] using the
    function [print] to print each element of [l].
    [prefix] is the current prefix string, but currently the position in the output channel [out]
    is at the beginning of a line (the prefix string is not already printed). *)
let print_list print prefix out l =
  let prefix' = prefix ^ String.make indentation ' ' in
  let rec print_list out = function
    | []  ->
       ()
    | [x] ->
       fprintf out "%s%s%a"
         prefix'
         branch_end
         (print (prefix' ^ " ")) x
    | x :: r ->
       fprintf out "%s%s%a\n%a"
         prefix'
         branch
         (print (prefix' ^ pipe)) x
         print_list r
  in
  print_list out l

(** [print_constant out c] prints the constant [c] on the output channel [out]. *)
let print_constant out = function
  | ConstBool b ->
     fprintf out "ConstBool %s" (string_of_bool b)
  | ConstInt i ->
     fprintf out "ConstInt %ld" i

(** [print_unop out op] prints the unary operator [op] on the output channel [out]. *)
let print_unop out = function
  | UOpNot ->
     fprintf out "UOpNot"

(** [print_binop out op] prints the binary operator [op] on the output channel [out]. *)
let print_binop out = function
  | OpAdd ->
     fprintf out "OpAdd"
  | OpSub ->
     fprintf out "OpSub"
  | OpMul ->
     fprintf out "OpMul"
  | OpLt  ->
     fprintf out "OpLt"
  | OpAnd ->
     fprintf out "OpAnd"

(** [print_expression prefix out e] prints the expression [e] on the output channel [out].
    [prefix] is the string already printed just before [e]. *)
let rec print_expression prefix out e =
  print_raw_expression prefix out (Location.content e) (Location.startpos e)

(** [print_raw_expression prefix out e pos] prints the expression [e] on the output channel [out].
    [prefix] is the string already printed just before [e].
    [pos] is the position informations of the beginning of the expression [e] in the source file.
    [pos] is used iff [show_location] is true. *)
and print_raw_expression prefix out e pos =
  let prefix' = prefix ^ String.make indentation ' ' in
  match e with
  | EConst c ->
     fprintf out "EConst (%a)" print_constant c;
     print_position out pos
  | EGetVar id ->
     fprintf out "EGetVar %a" print_identifier id
  | EUnOp (op, e) ->
     fprintf out "EUnOp %a" print_unop op;
     print_position out pos;
     fprintf out "\n%s%s%a"
       prefix'
       branch_end
       (print_expression prefix') e
  | EBinOp (op, e1, e2) ->
     fprintf out "EBinOp %a" print_binop op;
     print_position out pos;
     fprintf out "\n%s%s%a\n%s%s%a"
       prefix'
       branch
       (print_expression (prefix' ^ pipe)) e1
       prefix'
       branch_end
       (print_expression prefix') e2
  | EMethodCall (e1, id, args) ->
     fprintf out "EMethodCall";
     print_position out pos;
     fprintf out "\n%s%s%a\n%s%s%a\n%s%s%s%s%a"
       prefix'
       branch
       (print_expression (prefix' ^ pipe)) e1
       prefix'
       branch
       print_identifier id
       prefix'
       branch_end
       "()"
       (if args = [] then "" else "\n")
       (print_expression_list prefix') args
  | EArrayGet (e1, e2) ->
     fprintf out "EArrayGet";
     print_position out pos;
     fprintf out "\n%s%s%a\n%s%s%a"
       prefix'
       branch
       (print_expression (prefix' ^ pipe)) e1
       prefix'
       branch_end
       (print_expression prefix') e2
  | EArrayAlloc e ->
     fprintf out "EArrayAlloc";
     print_position out pos;
     fprintf out "\n%s%s%a"
       prefix'
       branch_end
       (print_expression prefix') e
  | EArrayLength e ->
     fprintf out "EArrayLength";
     print_position out pos;
     fprintf out "\n%s%s%a"
       prefix'
       branch_end
       (print_expression prefix') e
  | EThis ->
     fprintf out "EThis%a" print_position pos
  | EObjectAlloc id ->
     fprintf out "EObjectAlloc %a" print_identifier id

(** [print_expression_list prefix out l] prints the list of expressions [l] on the output channel [out].
    [prefix] is the current prefix string, but currently the position in the output channel [out] is
    at the beginning of a line (the prefix string is not already printed). *)
and print_expression_list prefix out l =
  print_list print_expression prefix out l

(** [print_instruction prefix out ins] prints the instruction [ins] on the output channel [out].
    [prefix] is the string already printed just before [ins]. *)
let rec print_instruction prefix out i =
  let prefix' = prefix ^ String.make indentation ' ' in
  match i with
  | ISyso e ->
     fprintf out "ISyso\n%s%s%a"
       prefix'
       branch_end
       (print_expression prefix') e
  | IBlock l ->
     fprintf out "IBlock\n%a"
       (print_instruction_list prefix) l
  | IIf (e, i1, i2) ->
     fprintf out "IIf\n%s%s%a\n%s%s%a\n%s%s%a"
       prefix'
       branch
       (print_expression (prefix' ^ pipe)) e
       prefix'
       branch
       (print_instruction (prefix' ^ pipe)) i1
       prefix'
       branch_end
       (print_instruction prefix') i2
  | IWhile (e, i) ->
     fprintf out "IWhile\n%s%s%a\n%s%s%a"
       prefix'
       branch
       (print_expression (prefix' ^ pipe)) e
       prefix'
       branch_end
       (print_instruction prefix') i
  | ISetVar (id, e) ->
     fprintf out "ISetVar\n%s%s%a\n%s%s%a"
       prefix'
       branch
       print_identifier id
       prefix'
       branch_end
       (print_expression prefix') e
  | IArraySet (id, e1, e2) ->
     fprintf out "IArraySet\n%s%s%a\n%s%s%a\n%s%s%a"
       prefix'
       branch
       print_identifier id
       prefix'
       branch
       (print_expression (prefix' ^ pipe)) e1
       prefix'
       branch_end
       (print_expression prefix') e2

(** [print_instruction_list prefix out l] prints the list of instructions [l] on the output channel [out].
    [prefix] is the current prefix string, but currently the position in the output channel [out] is
    at the beginning of a line (the prefix string is not already printed). *)
and print_instruction_list prefix out l =
  print_list print_instruction prefix out l

(** [print_type out typ] prints the type [typ] on the output channel [out]. *)
let print_type out typ =
  match typ with
  | TypInt ->
     fprintf out "int"
  | TypBool ->
     fprintf out "bool"
  | TypIntArray ->
     fprintf out "int[]"
  | Typ id ->
     fprintf out "%a" print_identifier id

(** [print_declaration_list out l] prints the list of declarations [l] on the output channel [out].
    A declaration is an identifier with its type. *)
let print_declaration_list out l =
  let print_declaration out (id, typ) =
    fprintf out "(%a, %a)"
      print_identifier id
      print_type typ
  in
  fprintf out "%a"
    (Print.sep_list Print.space print_declaration) l

(** [print_method prefix out m] prints the method [m] on the output channel [out].
    [prefix] is the string already printed just before [m]. *)
let print_method prefix out m =
  fprintf out "formals %a\n%s%sresult %a\n%s%slocals %a\n%s%sbody\n%a%s%s%sreturn\n%a"
    print_declaration_list m.formals
    prefix
    branch
    print_type m.result
    prefix
    branch
    print_declaration_list m.locals
    prefix
    branch
    (print_instruction_list (prefix ^ pipe)) m.body
    (if m.body = [] then "" else "\n")
    prefix
    branch_end
    (print_expression_list (prefix ^ " ")) [m.return]

(** [print_identifier_method_list prefix out l] prints the list of methods with their names [l] on the output channel [out].
    [prefix] is the current prefix string, but currently the position in the output channel [out] is
    at the beginning of a line (the prefix string is not already printed). *)
let print_identifier_method_list prefix out l =
  let print_identifier_method prefix out (id, m) =
    let prefix' = prefix ^ String.make indentation ' ' in
    fprintf out "%a\n%s%s%a"
      print_identifier id
      prefix'
      branch
      (print_method prefix') m
  in
  print_list print_identifier_method prefix out l

(** [print_class prefix out c] prints the class [c] on the output channel [out].
     [prefix] is the string already printed just before [c]. *)
let rec print_class prefix out c =
  fprintf out "extends ";
  (match c.extends with
     | None -> ()
     | Some id -> print_identifier out id);
  fprintf out "\n%s%sattributes %a\n%s%smethods\n%a"
    prefix
    branch
    print_declaration_list c.attributes
    prefix
    branch_end
    (print_identifier_method_list (prefix ^ " ")) c.methods

(** [print_identifier_class_list prefix out l] prints the list of classes with their names [l] on the output channel [out].
    [prefix] is the current prefix string, but currently the position in the output channel [out] is
    at the beginning of a line (the prefix string is not already printed). *)
and print_identifier_class_list prefix out l =
  let print_identifier_class prefix out (id, c) =
    let prefix' = prefix ^ String.make indentation ' ' in
    fprintf out "%a\n%s%s%a"
      print_identifier id
      prefix'
      branch
      (print_class prefix') c
  in
  print_list print_identifier_class prefix out l

let print out p show_loc =
  show_location := show_loc;
  let prefix = String.make indentation ' ' in
  fprintf out "program\n%s%sname %a\n%s%sdefs\n%a%s%s%smain_args %a\n%s%smain\n%a\n"
    prefix
    branch
    print_identifier p.name
    prefix
    branch
    (print_identifier_class_list (prefix ^ pipe)) p.defs
    (if p.defs = [] then "" else "\n")
    prefix
    branch
    print_identifier p.main_args
    prefix
    branch_end
    (print_instruction_list (prefix ^ " ")) [p.main]
