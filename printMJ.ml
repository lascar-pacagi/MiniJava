open Printf
open Print
open MJ

let indentation = 2

(** [constant out c] prints the constant [c] on the output channel [out]. *)
let constant out = function
  | ConstBool true ->
     fprintf out "true"
  | ConstBool false ->
     fprintf out "false"
  | ConstInt i ->
     fprintf out "%ld" i

(** [binop out op] prints the binary operator [op] on the output channel [out]. *)
let binop out = function
  | OpAdd ->
     fprintf out "+"
  | OpSub ->
     fprintf out "-"
  | OpMul ->
     fprintf out "*"
  | OpLt  ->
     fprintf out "<"
  | OpAnd ->
     fprintf out "&&"

(** [expr out e], [expr0 out e], ..., [expr6 out e] print the expression [e]
    on the output channel [out]. [expr] is a synonym for [expr6].
    We have different functions to minimize the number of parenthesis. An expression
    doesn't need parenthesis if the priority of its operands is greater or equal to
    its priority.
    [expr6] handles the expressions of least priority and [expr0] handles the expressions
    of greatest priority. It's in the default case of [expr0] that we put parenthesis, in
    this case, we have an expression of lower priority than the current context and so we
    have to put parenthesis around it and then call [expr] again. *)
let rec expr0 out = function
  | EConst c ->
     fprintf out "%a" constant c
  | EGetVar x ->
     fprintf out "%s" x
  | EThis ->
     fprintf out "this"
  | EMethodCall (o, c, es) ->
     fprintf out "%a.%s(%a)"
       expr0 o
       c
       (sep_list comma expr) es
  | EArrayGet (ea, ei) ->
     fprintf out "%a[%a]"
       expr0 ea
       expr ei
  | EArrayLength e ->
     fprintf out "%a.length"
       expr0 e
  | EObjectAlloc id ->
     fprintf out "new %s()"
       id
  | e ->
     fprintf out "(%a)"
       expr e

and expr1 out = function
  | EArrayAlloc e ->
     fprintf out "new int[%a]"
       expr e
  | e ->
     expr0 out e

and expr2 out = function
  | EUnOp (UOpNot, e) ->
     fprintf out "!%a"
       expr2 e
  | e ->
     expr1 out e

and expr3 out = function
  | EBinOp (OpMul as op, e1, e2) ->
     fprintf out "%a %a %a"
       expr3 e1
       binop op
       expr3 e2
  | e ->
     expr2 out e

and expr4 out = function
  | EBinOp (OpSub as op, e1, e2) ->
     fprintf out "%a %a %a"
       expr4 e1
       binop op
       expr3 e2
  | e ->
     expr3 out e

and expr5 out = function
  | EBinOp (OpAdd as op, e1, e2) ->
     fprintf out "%a %a %a"
       expr5 e1
       binop op
       expr5 e2
  | e ->
     expr4 out e

and expr6 out = function
  | EBinOp ((OpLt | OpAnd) as op, e1, e2) ->
     fprintf out "%a %a %a"
       expr6 e1
       binop op
       expr6 e2
  | e ->
     expr5 out e

and expr out e =
  expr6 out e

(** [binop out ins] prints the instruction [ins] on the output channel [out]. *)
let rec instr out = function
  | ISetVar (x, e) ->
     fprintf out "%s = %a;"
       x
       expr e
  | IArraySet (id, ei, ev) ->
     fprintf out "%s[%a] = %a;"
       id
       expr ei
       expr ev
  | IIf (c, i1, i2) ->
      fprintf out "if (%a) %a%telse %a"
        expr c
        instr i1
        nl
        instr i2
  | IWhile (c, i) ->
      fprintf out "while (%a) %a"
        expr c
        instr i
  | IBlock is ->
     fprintf out "{%a%t}"
       (indent indentation (sep_list nl instr)) is
       nl
  | ISyso e ->
     fprintf out "System.out.println(%a);"
       expr e

(** [typ out t] prints the type [t] on the output channel [out]. *)
let typ out = function
  | TypInt ->
     fprintf out "int"
  | TypBool ->
     fprintf out "boolean"
  | TypIntArray ->
     fprintf out "int[]"
  | Typ id ->
     fprintf out "%s"
       id

(** [typ out (x, t)] prints the type [t] and the associated variable [x] on the output channel [out]. *)
let binding out (x, t) =
  fprintf out "%a %s"
    typ t
    x

(** [metho out (name, m)] prints the method [name] with type [MJ.metho m] on the output channel [out]. *)
let metho out (name, m) =
  fprintf out "public %a %s(%a) {%a%a%t%t}"
    typ m.result
    name
    (sep_list comma binding) m.formals
    (term_list semicolon (indent indentation binding)) m.locals
    (list (indent indentation instr)) m.body
    (indent_t indentation (fun out -> fprintf out "return %a;" expr m.return))
    nl

(** [clas out (name, c)] prints the clas [name] with type [MJ.clas c] on the output channel [out]. *)
let clas out (name, c) =
  (match c.extends with
   | None ->
      fprintf out "class %s {%a%a%t}" name
   | Some class_name ->
      fprintf out "class %s extends %s {%a%a%t}" name class_name)
    (term_list semicolon (indent indentation binding)) c.attributes
    (list (indent indentation metho)) c.methods
    nl

let print_program out (p : MJ.program) : unit =
  fprintf out "class %s {%t%t}%t%a"
    p.name
    (indent_t indentation
       (fun out ->
         fprintf out "public static void main(String[] %s) {%a%t}"
           p.main_args
           (indent indentation instr) p.main
           nl))
    nl
    nl
    (sep_list nl clas) p.defs
