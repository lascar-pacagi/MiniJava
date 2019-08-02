open LMJ
open Printf

module SM = StringMap
module S = StringSet

type method_type = typ list * typ (** Parameters types and return type of a method. *)

type method_env = method_type SM.t

type attribute_env = typ SM.t

type class_type = attribute_env * method_env

type class_env = class_type SM.t

type variable_env = typ SM.t

exception Error of string

(** [error loc msg] raises an exception [Error] with the message [msg] and the
    position informations associated with [loc]. *)
let error (location : 'a Location.t) (msg : string) =
  raise (Error (sprintf "%s:\n%s"
                  (Error.positions (Location.startpos location) (Location.endpos location))
                  msg))

(** [error locs msg] raises an exception [Error] with the message [msg] and all
    the position informations of the list [locs]. *)
let errors (locations : 'a Location.t list) (msg : string) =
  raise (Error (sprintf "%s%s"
                  (List.fold_right (fun location acc ->
                      sprintf "%s:\n%s" (Error.positions (Location.startpos location) (Location.endpos location)) acc
                   ) locations "") msg))

(** [lookup msg id env] lookups the identifier [id] in the map [env].
    If the identifier is not present raises an error using the message [msg]. *)
let lookup (msg : string) (id : identifier) (env : 'a SM.t) =
  try
    SM.find (Location.content id) env
  with Not_found ->
    error id (sprintf "%s %s is undefined" msg (Location.content id))

(** [vlookup id env] lookups the variable [id] in the environment for variables (locals or parameters) [env]. *)
let vlookup : identifier -> variable_env -> typ = lookup "variable"

(** [mlookup m env] lookups the method [m] in the environment for methods [env]. *)
let mlookup : identifier -> method_env -> method_type = lookup "method"

(** [alookup a env] lookups the attribute [a] in the environment for attributes [env]. *)
let alookup : identifier -> attribute_env -> typ = lookup "attribute"

(** [clookup c env] lookups the class [c] in the environment for classes [env]. *)
let clookup : identifier -> class_env -> class_type = lookup "class"

(** [compatible t1 t2 instanceof] returns true iff the type [t1] is compatible with type [t2].
    For classes, uses the function [instanceof] to decide if [t1] is an instance of [t2]. *)
let rec compatible (typ1 : typ) (typ2 : typ) (instanceof : identifier -> identifier -> bool) : bool =
  match typ1, typ2 with
  | TypInt, TypInt
  | TypBool, TypBool
  | TypIntArray, TypIntArray -> true
  | Typ t1, Typ t2 -> instanceof t1 t2
  | _, _ -> false

(** [type_to_string t] converts the type [t] into a string representation. *)
let rec type_to_string : typ -> string = function
  | TypInt -> "integer"
  | TypBool -> "boolean"
  | TypIntArray -> "int[]"
  | Typ t -> Location.content t

(** [typecheck_call cenv venv vinit instanceof o callee es] checks, using the environments [cenv] and [venv],
    the set of initialized variables [vinit] and the [instanceof] function, that
     * the expression [o] is an object of type [t],
     * the method [callee] belongs to the class [t],
     * the parameters [es] are compatibles with the types of the formal parameters.
    If [typecheck_call] succeeds, the return type of [callee] is returned. *)
let rec typecheck_call (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (o : expression)
    (callee : identifier)
    (expressions : expression list) : typ =
  let o_type = typecheck_expression cenv venv vinit instanceof o in
  match o_type with
  | Typ t ->
    begin
      let _, method_env = clookup t cenv in
      let (formals : typ list), (result : typ) = mlookup callee method_env in
      try
        List.iter2 (typecheck_expression_expecting cenv venv vinit instanceof) formals expressions;
        result
      with Invalid_argument _ ->
        error callee
          (sprintf "Invalid function call, expected %d arguments, got %d"
             (List.length formals)
             (List.length expressions))
    end
  | _ -> error o (sprintf "A class is expected, got %s" (type_to_string o_type))


(** [typecheck_expression_expecting cenv venv vinit instanceof typ1 e] checks, using the
    environments [cenv] and [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the expression [e] has a type compatible with type [typ1]. *)
and typecheck_expression_expecting (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (typ1 : typ)
    (e : expression) : unit =
  let typ2 = typecheck_expression cenv venv vinit instanceof e in
  if not (compatible typ2 typ1 instanceof) then
    error e
      (sprintf "Type mismatch, expected %s, got %s" (type_to_string typ1) (type_to_string typ2))

(** [typecheck_expression cenv venv vinit instanceof e] checks, using the environments [cenv] and
    [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the expression [e] is well typed.
    If [typecheck_expression] succeeds, the type of [e] is returned. *)
and typecheck_expression (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (e : expression) : typ =
  match Location.content e with
  | EConst (ConstBool _) -> TypBool

  | EConst (ConstInt _) -> TypInt

  | EGetVar v ->
     let typ = vlookup v venv in
     let v' = Location.content v in
     if not (S.mem v' vinit) then
       error v (sprintf "Variable %s has not been initialized" v');
     typ

  | EUnOp (op, e) ->
      let expected, returned =
        match op with
        | UOpNot -> TypBool, TypBool
      in
      typecheck_expression_expecting cenv venv vinit instanceof expected e;
      returned

  | EBinOp (op, e1, e2) ->
      let expected, returned =
        match op with
        | OpAdd
        | OpSub
        | OpMul -> TypInt, TypInt
        | OpLt  -> TypInt, TypBool
        | OpAnd -> TypBool, TypBool
      in
      typecheck_expression_expecting cenv venv vinit instanceof expected e1;
      typecheck_expression_expecting cenv venv vinit instanceof expected e2;
      returned

  | EMethodCall (o, callee, expressions) ->
     typecheck_call cenv venv vinit instanceof o callee expressions

  | EArrayGet (earray, eindex) ->
    typecheck_expression_expecting cenv venv vinit instanceof TypInt eindex;
    typecheck_expression_expecting cenv venv vinit instanceof TypIntArray earray;
    TypInt

  | EArrayAlloc elength ->
    typecheck_expression_expecting cenv venv vinit instanceof TypInt elength;
    TypIntArray

  | EArrayLength earray ->
    typecheck_expression_expecting cenv venv vinit instanceof TypIntArray earray;
    TypInt

  | EThis ->
     vlookup (Location.make (Location.startpos e) (Location.endpos e) "this") venv

  | EObjectAlloc id ->
      clookup id cenv |> ignore;
      Typ id

(** [typecheck_instruction cenv venv vinit instanceof inst] checks, using the environments [cenv] and
    [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the instruction [inst] is well typed.
    If [typecheck_instruction] succeeds, the new set of initialized variables is returned. *)
let rec typecheck_instruction (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (inst : instruction) : S.t =
  match inst with
  | ISetVar (v, e) ->
     let vinit =
       S.add (Location.content v) vinit
     in
     typecheck_expression_expecting cenv venv vinit instanceof (vlookup v venv) e;
     vinit

  | IArraySet (earray, eindex, evalue) ->
    typecheck_expression_expecting cenv venv vinit instanceof TypIntArray
      (Location.make (Location.startpos earray) (Location.endpos earray) (EGetVar earray));
    typecheck_expression_expecting cenv venv vinit instanceof TypInt eindex;
    typecheck_expression_expecting cenv venv vinit instanceof TypInt evalue;
    vinit

  | IBlock instructions ->
     List.fold_left
       (fun vinit inst ->
         typecheck_instruction cenv venv vinit instanceof inst)
       vinit
       instructions

  | IIf (cond, ithen, ielse) ->
    typecheck_expression_expecting cenv venv vinit instanceof TypBool cond;
    let vinit1 =
      typecheck_instruction cenv venv vinit instanceof ithen
    in
    let vinit2 =
      typecheck_instruction cenv venv vinit instanceof ielse
    in
    S.inter vinit1 vinit2

  | IWhile (cond, ibody) ->
    typecheck_expression_expecting cenv venv vinit instanceof TypBool cond;
    typecheck_instruction cenv venv vinit instanceof ibody

  | ISyso e ->
     typecheck_expression_expecting cenv venv vinit instanceof TypInt e;
     vinit

(** [occurences x bindings] returns the elements in [bindings] that have [x] has identifier. *)
let occurrences (x : string) (bindings : (identifier * 'a) list) : identifier list =
  List.map fst (List.filter (fun (id, _) -> x = Location.content id) bindings)

(** [map_of_association_list entity bindings] creates a map from the association list [bindings].
    If some identifiers are duplicated, [map_of_association_list] raises an [Error] exception,
    using the string [entity] in the error message. *)
let map_of_association_list (entity : string) (bindings : (identifier * 'a) list) : 'a SM.t =
  try
    SM.of_association_list (List.map (fun (id, data) -> (Location.content id, data)) bindings)
  with SM.Duplicate x ->
    errors (occurrences x bindings) (sprintf "%s %s is declared more than once" entity x)

(** [variable_map decls] creates an environment for variables using the association list [decls]. *)
let variable_map (decls : (identifier * typ) list) : variable_env =
  map_of_association_list "Variable" decls

(** [method_map decls] creates an environment for methods using the association list [decls]. *)
let method_map (decls : (identifier * method_type) list) : method_env =
  map_of_association_list "Method" decls

(** [typecheck_method cenv venv instanceof m] checks, using the environments [cenv] and [venv]
    and the [instanceof] function, that the method [m] is well typed. *)
let typecheck_method (cenv : class_env) (venv : variable_env)
    (instanceof : identifier -> identifier -> bool)
    (m : metho) : unit =

  let formals = m.formals
  and locals = m.locals in

  let mformals = variable_map formals
  and mlocals = variable_map locals in

  begin
    try
      let x =
        StringSet.choose
          (StringSet.inter
             (SM.domain mformals)
             (SM.domain mlocals))
      in
      errors (occurrences x formals @ occurrences x locals)
        "A formal parameter and a local variable cannot carry the same name"
    with Not_found ->
      ()
  end;

  let venv =
    SM.addm mformals venv
  |> SM.addm mlocals
  in

  let vinit =
    S.diff (SM.domain venv) (SM.domain mlocals)
  in
  let vinit =
    typecheck_instruction cenv venv vinit instanceof (IBlock m.body)
  in
  typecheck_expression_expecting cenv venv vinit instanceof m.result m.return

(** [typecheck_class cenv instanceof (name, c)] checks, using the environments [cenv] and [venv]
    and the [instanceof] function, that the class [name] with type [c] is well typed. *)
let typecheck_class (cenv : class_env) (instanceof : identifier -> identifier -> bool)
    ((name, c) : identifier * clas) : unit =
  let attribute_env, _ = clookup name cenv in
  let venv = SM.add "this" (Typ name) attribute_env in
  List.iter (typecheck_method cenv venv instanceof) (List.map snd c.methods)

(** [extract_method_type m] creates a [method_type] from the method [m]. *)
let extract_method_type (m : metho) : method_type =
  (List.map snd m.formals, m.result)

(** [extract_class_type c] creates a [class_type] from the class [c]. *)
let extract_class_type (c : clas) : class_type =
  (variable_map c.attributes,
   method_map (List.map (fun (id, m) -> (id, extract_method_type m)) c.methods))

(** [class_map decls] creates an environment for classes using the association list [decls]. *)
let class_map (decls : (identifier * clas) list) : clas SM.t =
  map_of_association_list "Class" decls

(** [create_instancef cmap] creates an [instanceof] function such that
    [instanceof id1 id2] is true iff class [id2] is a parent (direct or indirect)
    of class [id1]. *)
let create_instanceof (cmap : clas SM.t) : identifier -> identifier -> bool =
  let rec instanceof id1 id2 =
    if id1 = id2 then true
    else
      try
        match (SM.find id1 cmap).extends with
        | None -> false
        | Some id3 -> instanceof (Location.content id3) id2
      with Not_found -> false
  in
  fun id1 id2 ->
    instanceof (Location.content id1) (Location.content id2)
  (* let memo = Hashtbl.create 97 in *)
  (* fun id1 id2 -> *)
  (*   let id1', id2' = Location.content id1, Location.content id2 in *)
  (*   try *)
  (*     Hashtbl.find memo (id1', id2') *)
  (*   with Not_found -> *)
  (*     let res = instanceof id1' id2' in *)
  (*     Hashtbl.add memo (id1', id2') res; *)
  (*     res *)

(** [add_method cmap instanceof] completes each class in [cmap] by creating a new map where we add
    to a given class the methods and attributes of its parents. If a method in a parent class has
    the same name than a method in a subclass, we check that the later overrides the former. *)
let add_method
      (cmap : clas SM.t)
      (instanceof : identifier -> identifier -> bool)
    : clas SM.t =
  let test_compatible_signature ((name, m) : identifier * metho) ((name', m') : identifier * metho) : unit =
    let typecheck_params (typ : typ) (typ' : typ) : unit =
      if not (compatible typ typ'
                (fun t1 t2 -> Location.content t1 = Location.content t2))
      then
        errors [name; name']
          (sprintf "Type mismatch in params of overriden method, expected %s, got %s" (type_to_string typ) (type_to_string typ'))
    in
    let typecheck_result (typ : typ) (typ' : typ) : unit =
      if not (compatible typ' typ instanceof) then
          errors [name; name']
            (sprintf "Type mismatch in result of overriden method, expected %s, got %s" (type_to_string typ) (type_to_string typ'))
    in
    let formals, result = extract_method_type m
    and formals', result' = extract_method_type m' in
    try
      List.iter2 typecheck_params formals formals';
      typecheck_result result result'
    with Invalid_argument _ ->
      errors [name; name']
        (sprintf "A function that overrides another one must have the same number of parameters" )
  in
  (**
    [complete o c] adds to the class [c] all methods and attributes of its parents starting from direct parent [o].
    It checks if an overriden method (a method already defined with the same name in a parent class)
    is correctly typed: same parameters and a return type that is compatible with the overriden method.
    When there exists attributes with the same name in a parent class, we only keep the ones from the subclass.
  *)
  let rec complete (parent : identifier option) (c : clas) : clas =
    match parent with
    | None -> c
    | Some id ->
      let c' = SM.find (Location.content id) cmap in
      complete c'.extends
        {
          c with
            attributes =
            (List.filter
               (fun (name, _) ->
                 not (List.exists (fun (name', _) -> Location.content name = Location.content name') c.attributes)
               )
               c'.attributes) @ c.attributes;

            methods =
            (List.filter
               (fun (name, m) ->
                 try
                   List.find (fun (name', _) -> Location.content name = Location.content name') c.methods
                   |> test_compatible_signature (name, m);
                   false
                 with Not_found -> true
               )
               c'.methods) @ c.methods
        }
  in
  SM.map
    (fun c -> complete c.extends c)
    cmap

let typecheck_program (p : program) : unit =
  let cmap = class_map p.defs in
  let instanceof = create_instanceof cmap in
  let cenv =
    add_method cmap instanceof
      |> SM.map extract_class_type
  in
  List.iter (typecheck_class cenv instanceof) p.defs;
  let venv = SM.singleton "this" (Typ p.name) in
  typecheck_instruction cenv venv S.empty instanceof p.main
  |> ignore
