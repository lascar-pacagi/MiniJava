open Printf
open Print
open MJ

(** [struct_array_name] is the name of the structure that holds an array and its length:
    class array {
          int* array;
          int length;
    };
    But [array] could be the name of a class and in this case, we have to create a new name. *)
let struct_array_name = ref ""

(** [name1] is a fresh name, different from all other variables in the MiniJava program. *)
let name1 = ref ""

(** [name2] is a fresh name, different from all other variables in the MiniJava program,
    and different from [name1]. *)
let name2 = ref ""

let indentation = 2

(** If we have the following class hierarchy
       class A {}
       class B extends A {}
       class C extends B {}
    and [defs] such that
        [a = List.assoc "A" defs]
    and [b = List.assoc "B" defs]
    and [c = List.assoc "C" defs]
    then [fold_class_hierarchy f defs (Some "C") acc] is [f "C" c (f "B" b (f "A" a acc))]. *)
let rec fold_class_hierarchy
          (f : string -> MJ.clas -> 'a -> 'a)
          (defs : (MJ.identifier * MJ.clas) list)
          (parent : string option)
          (acc : 'a)
        : 'a =
  match parent with
  | None -> acc
  | Some name ->
     try
       let c = List.assoc name defs in
       f name c (fold_class_hierarchy f defs c.extends acc)
     with Not_found ->
       acc

module SM = StringMap

module type ClassInfoType = sig
  type t
  (** [create name c defs] creates a [ClassInfoType.t] from the [name] of a class,
      the class [c] and all class definitions [defs]. *)
  val create : string -> MJ.clas -> (identifier * clas) list -> t

  (** [class_name class_info] returns the name of the class of this [class_info]. *)
  val class_name : t -> string

  (** [is_attribute m v class_info] checks if the variable [v] in the context of
     method [m] is an attribute of the [class_info] or not (if not it is a parameter or a local variable). *)
  val is_attribute : string -> string -> t -> bool

  (** [class_of m v class_info] returns the class name of variable [v] in the context of the
     method [m] in [class_info]. If [v] is of primitive type, the function returns the empty string. *)
  val class_of : string -> string -> t -> string

  (** [method_class_origin m class_info] returns the name of the class, in the class hierarchy for
      this [class_info], where the method [m] is last defined. *)
  val method_class_origin : string -> t -> string

  (** [attribute_class_origin a class_info] returns the name of the class, in the class hierarchy for this [class_info],
      where the attribute [a] is last defined. *)
  val attribute_class_origin : string -> t -> string

  (** [vtable_index m class_info] returns the index of the method [m] in the virtual table for this [class_info]. *)
  val vtable_index : string -> t -> int

  (** [return_type m class_info] gets the return type of the method [m] for this [class_info]. *)
  val return_type : string -> t -> MJ.typ

  (** [get_methods class_info] returns all the method names of the [class_info] in a list.
      A method name is prefixed by the class where the method is last defined. The list is
      sorted by increasing method's vtable indices. *)
  val get_methods : t -> string list

  (** [get_attributes class_info] returns a list of all the attribute names of the [class_info] associated with their types.
      An attribute name is prefixed by the class where it is defined.
      The order in this list is such that the attributes for a parent class are put before the attributes of a child class.*)
  val get_attributes : t -> (string * MJ.typ) list
end

module ClassInfo : ClassInfoType = struct
  (** If we have the following classes
    class A {
      int a;
    }
    class B extends A {
      boolean a;
      int b;
    }
   [attribute_info] is the following map for class B:
     "a" --> ("A", 0, int), ("B", 1, boolean)
     "b" --> ("B", 2, int)
   The first element of the triple is the class origin of the attribute,
   the second element is an index used to create the field of the corresponding
   C structure in a correct order and the third element is the type of the attribute. *)
  type attribute_info = (string * int * MJ.typ) list SM.t

  (** If we have the following classes
    class A {
      public int m1() { return 1; }
      public int m2() { return 2; }
    }
    class B extends A {
      public int m1() { return 3; }
      public int m3() { return 4; }
    }
   [method_info] is the following map for class B:
     "m1" --> ("B", 0, MJ.metho for m1)
     "m2" --> ("A", 1, MJ.metho for m2)
     "m3" --> ("B", 2, MJ.metho for m3)
   The first element of the triple is the class origin of the method,
   the second element is the virtual table index and
   the third element is the [MJ.metho] type for the method. *)
  type method_info = (string * int * MJ.metho) SM.t

  type t = {
      class_name : string;
      attribute_info : attribute_info;
      method_info : method_info
    }

  let create class_name c defs =
    let attribute_info =
      (** If a parent class and a child class have the same attribute name, the attribute
            of the child class is before the attribute of the parent class in the list. *)
      let update class_name index typ = function
        | None -> Some [class_name, index, typ]
        | Some l -> Some ((class_name, index, typ) :: l)
      in
      (** The [index] is used to sort the attributes in the C structure. The attributes of a child
            must come after the attributes of the parent. *)
      let index = ref 0 in
      fold_class_hierarchy
        (fun class_name clas acc ->
          let res =
            List.fold_left
              (fun acc (attribute_name, t) ->
                SM.update attribute_name (update class_name !index t) acc
              )
              acc
              clas.attributes
          in
          incr index;
          res
        )
        defs
        (Some class_name)
        SM.empty
    in
    let method_info =
      fold_class_hierarchy
        (fun class_name clas acc ->
          let n = ref (SM.cardinal acc) in
          List.fold_left
            (fun acc (method_name, m) ->
              if SM.mem method_name acc then
                (** If we have the following classes
                          class A {
                            public int m1() { return 1; }
                            public int m2() { return 2; }
                          }
                          class B extends A {
                            public int m1() { return 3; }
                            public int m3() { return 4; }
                          }
                          class C extends B {
                            public int m1() { return 5; }
                            public int m3() { return 6; }
                          }
                      To handle dynamic binding, [A.m1], [B.m1] and [C.m1] must have the same vtable index.
                      [B.m3] and [C.m3] must also have the same vtable index. *)
                let _, index, _ = SM.find method_name acc in
                SM.add method_name (class_name, index, m) acc
              else
                begin
                  let index = !n in
                  incr n;
                  SM.add method_name (class_name, index, m) acc
                end)
            acc
            clas.methods
        )
        defs
        (Some class_name)
        SM.empty
    in
    { class_name = class_name;
      attribute_info = attribute_info;
      method_info = method_info }

  let class_name class_info =
    class_info.class_name

  (** [find_variable_type m v class_info] gets the type of a variable [v] (formal parameter or local variable)
      for the method [m] in [class_info]. If the variable doesn't exist, raises [Not_found]. *)
  let find_variable_type m v class_info =
    let _, _, metho = SM.find m class_info.method_info in
    match List.assoc_opt v metho.formals with
    | Some t -> t
    | None -> List.assoc v metho.locals

  let is_attribute m v class_info =
    try
      find_variable_type m v class_info
      |> ignore;
      false
    with Not_found -> true

  let class_of m v class_info =
    if is_attribute m v class_info then
      let _, _, t =
        SM.find v class_info.attribute_info
        |> List.hd
      in
      match t with
      | Typ t -> t
      | _ -> ""
    else
      match find_variable_type m v class_info with
      | Typ t -> t
      | _ -> ""

  let method_class_origin m class_info =
    let orig, _, _ = SM.find m class_info.method_info in
    orig

  let attribute_class_origin v class_info =
    let orig, _, _ = SM.find v class_info.attribute_info |> List.hd in
    orig

  let vtable_index m class_info =
    let _, index, _ = SM.find m class_info.method_info in
    index

  let return_type m class_info =
    let _, _, metho = SM.find m class_info.method_info in
    metho.result

  let get_methods class_info =
    SM.fold
      (fun method_name (orig, index, _) acc ->
        (index, orig ^ "_" ^ method_name) :: acc
      )
      class_info.method_info
      []
    |> List.sort compare
    |> List.map snd

  let get_attributes class_info =
    SM.fold
      (fun attribute_name l acc ->
        List.fold_right
          (fun (orig, index, t) acc ->
            (index, attribute_name, orig, t) :: acc)
          l
          acc
      )
      class_info.attribute_info
      []
    |> List.sort compare
    |> List.map (fun (_, attribute_name, orig, t) -> (orig ^ "_" ^ attribute_name, t))
end

(** [class_infos] is a hash table from the name of a class to a [class_info]. *)
let class_infos = Hashtbl.create 57

(** [init_class_infos p] fills the [class_infos] hash table using the classes defined in [p]. *)
let init_class_infos (p : MJ.program) : unit =
  let main =
    {
      extends = None;
      attributes = [];
      methods = []
    }
  in
  ClassInfo.create p.name main p.defs
  |> Hashtbl.add class_infos p.name;
  (** For each class in the program [p] we create a [class_info] and we add it to
      the hash table [class_infos]. *)
  List.iter
    (fun (class_name, clas) ->
      ClassInfo.create class_name clas p.defs
      |> Hashtbl.add class_infos class_name
    )
    p.defs

(** [get_class_info c] returns the [class_info] associated with class name [c]. *)
let get_class_info (c : string) : ClassInfo.t =
  Hashtbl.find class_infos c

(** [constant2c out c] transpiles the constant [c] to C on the output channel [out]. *)
let constant2c
      out
      (c : MJ.constant)
    : unit =

  match c with
  | ConstBool true  -> fprintf out "1"
  | ConstBool false -> fprintf out "0"
  | ConstInt i      -> fprintf out "%ld" i

(** [binop2c out op] transpiles the binary operator [op] to C on the output channel [out]. *)
let binop2c
      out
      (op : MJ.binop)
    : unit =
  match op with
  | OpAdd -> fprintf out "+"
  | OpSub -> fprintf out "-"
  | OpMul -> fprintf out "*"
  | OpLt  -> fprintf out "<"
  | OpAnd -> fprintf out "&&"

(** [type2c out typ] transpiles the type [typ] to C on the output channel [out]. *)
let type2c
      out
      (typ : MJ.typ)
    : unit =
  match typ with
  | TypInt -> fprintf out "int"
  | TypBool -> fprintf out "int"
  | TypIntArray -> fprintf out "struct %s*" !struct_array_name
  | Typ t -> fprintf out "struct %s*" t

(** [cast out typ] transpiles the cast to [typ] to C on the output channel [out]. *)
let cast
      out
      (typ : MJ.typ)
    : unit =
  fprintf out "(%a)" type2c typ

(** [var2c m class_info v] transpiles the variable [v] in the context of [class_info] and method [m]
    to C on the output channel [out].
    We must distinguish between an attribute and a local variable or a parameter. *)
let var2c
      (method_name : string)
      (class_info : ClassInfo.t)
      out
      (v : string)
    : unit =
  if ClassInfo.is_attribute method_name v class_info then
    let class_origin = ClassInfo.attribute_class_origin v class_info in
    fprintf out "this->%s_%s" class_origin v
  else fprintf out "%s" v

(** [get_class m class_info e] gets the class name of the the type of expression [e] in the context
    of method [m] in [class_info]. If no class type is associated with expression [e], [get_class]
    returns the empty string. *)
let rec get_class
          (method_name : string)
          (class_info : ClassInfo.t)
          (e : MJ.expression)
        : string =
  match e with
  | EGetVar x -> ClassInfo.class_of method_name x class_info

  | EMethodCall (o, m, _) ->
     begin
       let typ =
         get_class method_name class_info o
         |> get_class_info
         |> ClassInfo.return_type m
       in
       match typ with
       | Typ t -> t
       | _ -> ""
     end

  | EThis -> ClassInfo.class_name class_info

  | EObjectAlloc id -> id

  | _ -> ""

(** [expr2c m class_info out e] transpiles the expression [e], in the context of method [m] and [class_info],
    to C on the output channel [out]. *)
let expr2c
      (method_name : string)
      (class_info : ClassInfo.t)
      out
      (expr : MJ.expression)
    : unit =
  let rec expr2c out e =
    match e with
    | EConst const ->
       fprintf out "%a" constant2c const

    | EGetVar v ->
       var2c method_name class_info out v

    | EThis ->
       fprintf out "this"

    | EMethodCall (o, callee, args) ->
       let clas = get_class method_name class_info o in
       let class_info = get_class_info clas in
       let index = ClassInfo.vtable_index callee class_info in
       let typ = ClassInfo.return_type callee class_info in
       fprintf out "({ struct %s* %s = %a; %a %s->vtable[%d](%s%a); })"
         clas
         !name1
         expr2c o
         cast typ
         !name1
         index
         !name1
         (prec_list comma expr2c) args

    | EArrayAlloc e ->
       fprintf out "(void*)({ int %s = %a; \
                    if (%s < 0) exit(1); \
                    struct %s* res = tgc_alloc(({ extern tgc_t gc; &gc; }), sizeof(struct %s)); \
                    res->array = (int*) tgc_calloc(({ extern tgc_t gc; &gc; }), %s, sizeof(int)); \
                    res->length = %s; res; })"
         !name1
         expr2c e
         !name1
         !struct_array_name
         !struct_array_name
         !name1
         !name1

    | EObjectAlloc id ->
       fprintf out "({ struct %s* res = tgc_calloc(({ extern tgc_t gc; &gc; }), 1, sizeof(*res)); \
                    res->vtable = %s_vtable; \
                    res; })"
         id
         id

    | EArrayGet (ea, ei) ->
       fprintf out "({ int %s = %a; \
                    struct %s* %s = %a; \
                    int res; \
                    if (%s < 0 || %s >= %s->length) exit(1); \
                    else res = %s->array[%s]; res; })"
         !name1
         expr2c ei
         !struct_array_name
         !name2
         expr2c ea
         !name1
         !name1
         !name2
         !name2
         !name1

    | EArrayLength e ->
       fprintf out "(%a)->length"
         expr2c e

    | EUnOp (UOpNot, e) ->
       fprintf out "!(%a)"
         expr2c e

    | EBinOp (op, e1, e2) ->
       fprintf out "(%a %a %a)"
         expr2c e1
         binop2c op
         expr2c e2
  in
  expr2c out expr

(** [instr2c m class_info out ins] transpiles the instruction [ins], in the context of method [m] and [class_info],
    to C on the output channel [out]. *)
let instr2c
      (method_name : string)
      (class_info : ClassInfo.t)
      out
      (ins : MJ.instruction)
    : unit =
  let rec instr2c out ins =
    match ins with
    | ISetVar (x, e) ->
       let x_class = ClassInfo.class_of method_name x class_info in
       let e_class = get_class method_name class_info e in
       fprintf out "%a = %s%a;"
         (var2c method_name class_info) x
         (if x_class <> e_class then sprintf "(struct %s*) " x_class else "")
         (expr2c method_name class_info) e

    | IArraySet (id, ei, ev) ->
       fprintf out "(%a)->array[%a] = %a;"
         (var2c method_name class_info) id
         (expr2c method_name class_info) ei
         (expr2c method_name class_info) ev

    | IIf (c, i1, i2) ->
       fprintf out "if (%a) %a%telse %a"
         (expr2c method_name class_info) c
         instr2c i1
         nl
         instr2c i2

    | IWhile (c, i) ->
       fprintf out "while (%a) %a"
         (expr2c method_name class_info) c
         instr2c i

    | IBlock is ->
       fprintf out "{%a%t}"
         (indent indentation (sep_list nl instr2c)) is
         nl

    | ISyso e ->
       fprintf out "printf(\"%%d\\n\", %a);"
         (expr2c method_name class_info) e
  in
  instr2c out ins

(** [class_declaration2c out c] transpiles the name of a class [c] to a C structure declaration
    on the output channel [out]. *)
let class_declaration2c
      out
      (class_name : string)
    : unit =
  fprintf out "struct %s;" class_name

(** [decl2c out (id, t)] transpiles the declaration [(id, t)] to C on the output channel [out]. *)
let decl2c
      out
      ((id, t) : string * MJ.typ)
    : unit =
  fprintf out "%a %s"
    type2c t
    id

(** [method_declaration2c out (name, c)] transpiles all the declarations of the methods of the class [name] with type [c]
    to C on the output channel [out]. *)
let method_declaration2c
      out
      ((class_name, clas) : string * MJ.clas)
    : unit =
  let method_declaration2c
        out
        ((method_name, m) : string * MJ.metho)
      : unit =
    fprintf out "void* %s_%s(struct %s* this%a);"
      class_name
      method_name
      class_name
      (prec_list comma decl2c)
      m.formals
  in
  fprintf out "%a"
    (sep_list nl method_declaration2c)
    clas.methods

(** [class_definition2c out (name, c)] defines the C structure representing the class [name] with type [c] on the output channel [out]. *)
let class_definition2c
      out
      ((class_name, clas) : string * MJ.clas)
    : unit =
  let field_names =
    get_class_info class_name
    |> ClassInfo.get_attributes
  in
  let field2c
        out
        ((name, t) : string * MJ.typ)
      : unit =
    fprintf out "%a %s"
      type2c t
      name
  in
  fprintf out "struct %s {%t%a\n};"
    class_name
    (indent_t indentation (fun out -> fprintf out "void* (**vtable)();"))
    (term_list semicolon (indent indentation field2c)) field_names

(** [method_definition2c out (name, c)] transpiles all the definitions of the methods of the class [name] with type [c]
    to C on the output channel [out]. *)
let method_definition2c
      out
      ((class_name, clas) : string * MJ.clas)
    : unit =
  let class_info = get_class_info class_name in
  let method_definition out (method_name, m) =
    let return2c out e =
      fprintf out "return (void*)(%a);"
        (expr2c method_name class_info) e
    in
    fprintf out "void* %s_%s(struct %s* this%a) {%a%a%a\n}"
      class_name
      method_name
      class_name
      (prec_list comma decl2c) m.formals
      (term_list semicolon (indent indentation decl2c))
      m.locals
      (list (indent indentation (instr2c method_name class_info))) m.body
      (indent indentation return2c) m.return
  in
  fprintf out "%a"
    (sep_list nl method_definition)
    clas.methods

(** [vtable_definition2c out c] creates the virtual tables for all the methods of class [c]
    on the output channel [out]. *)
let vtable_definition2c
      out
      class_name
    : unit =
  let class_info = get_class_info class_name in
  fprintf out "void* (*%s_vtable[])() = { %a };"
    class_name
    (sep_list comma print_string)
    (ClassInfo.get_methods class_info)

(** [all_variables p] returns the list of all the variables of program [p]. *)
let all_variables (p : MJ.program) : string list =
  let variables_from_method (m : MJ.metho) : string list =
    List.(map fst m.formals
          @ map fst m.locals)
  in
  p.main_args ::
    List.(map
            (fun (_, clas) ->
              map fst clas.attributes
              @ (map snd clas.methods
                 |> map variables_from_method
                 |> flatten))
            p.defs
          |> flatten)

let program2c out (p : MJ.program) : unit =
  init_class_infos p;
  let all_class_names =
    List.map fst p.defs
  in
  let rec variant s l =
    if List.mem s l then
      variant (s ^ "_") l
    else
      s
  in
  struct_array_name := variant "array" all_class_names;
  let all_variables = all_variables p in
  name1 := variant "tmp1" all_variables;
  name2 := variant "tmp2" (!name1 :: all_variables);
  fprintf out
    "#include <stdio.h>\n\
     #include <stdlib.h>\n\
     #include \"tgc.h\"\n\
     #pragma GCC diagnostic ignored \"-Wpointer-to-int-cast\"\n\
     #pragma GCC diagnostic ignored \"-Wint-to-pointer-cast\"\n\
     struct %s { int* array; int length; };\n\
     tgc_t gc;\n\
     %a\
     %a\
     %a\
     %a\
     %a\
     int main(int argc, char *argv[]) {\
     %a\
     %a\
     %a\n\
     %a\n\
     }\n"
    !struct_array_name

    (term_list nl class_declaration2c)
    all_class_names

    (term_list nl method_declaration2c)
    (List.filter (fun (_, c) -> c.methods <> []) p.defs)

    (term_list nl class_definition2c)
    p.defs

    (term_list nl vtable_definition2c)
    all_class_names

    (term_list nl method_definition2c)
    (List.filter (fun (_, c) -> c.methods <> []) p.defs)

    (indent indentation print_string) "tgc_start(&gc, &argc);"

    (indent indentation (instr2c "main" (get_class_info p.name)))
    p.main

    (indent indentation print_string) "tgc_stop(&gc);"

    (indent indentation print_string) "return 0;"
