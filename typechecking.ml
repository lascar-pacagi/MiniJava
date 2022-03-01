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

(** [typ_lmj_to_tmj t] converts the [LMJ] type [t] into the equivalent [TMJ] type. *)
let rec type_lmj_to_tmj = function
  | TypInt      -> TMJ.TypInt
  | TypBool     -> TMJ.TypBool
  | TypIntArray -> TMJ.TypIntArray
  | Typ id      -> TMJ.Typ (Location.content id)

(** [typ_tmj_to_lmj s e t] converts the [TMJ] type [t] into the equivalent [LMJ] type using location starting position [s] and location ending position [e]. *)
let rec type_tmj_to_lmj startpos endpos = function
| TMJ.TypInt      -> TypInt
| TMJ.TypBool     -> TypBool
| TMJ.TypIntArray -> TypIntArray
| TMJ.Typ id      -> Typ (Location.make startpos endpos id)

(** [tmj_type_to_string t] converts the [TMJ] type [t] into a string representation. *)
let rec tmj_type_to_string : TMJ.typ -> string = function
  | TMJ.TypInt -> "integer"
  | TMJ.TypBool -> "boolean"
  | TMJ.TypIntArray -> "int[]"
  | TMJ.Typ t -> t

(** [type_to_string t] converts the [LMJ] type [t] into a string representation. *)
let rec type_to_string (typ : LMJ.typ) : string =
  type_lmj_to_tmj typ
  |> tmj_type_to_string

(** [mke r t] creates a [TMJ] expression with raw expression [r] and type [t]. *)
let mke raw_expression typ = TMJ.{ raw_expression; typ = type_lmj_to_tmj typ }

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
    (expressions : expression list) : TMJ.expression =
  let o' = typecheck_expression cenv venv vinit instanceof o in
  match o'.typ with
  | Typ t ->
    begin
      let _, method_env = Location.(clookup (make (startpos o) (endpos o) t) cenv) in
      let (formals : typ list), (result : typ) = mlookup callee method_env in
      try
        let expressions' =
          List.fold_left2 (fun acc formal e -> typecheck_expression_expecting cenv venv vinit instanceof formal e :: acc) [] formals expressions
          |> List.rev 
        in
        mke (TMJ.EMethodCall (o', Location.content callee, expressions')) result
      with Invalid_argument _ ->
        error callee
          (sprintf "Invalid function call, expected %d arguments, got %d"
             (List.length formals)
             (List.length expressions))
    end
  | _ -> error o (sprintf "A class is expected, got %s" (tmj_type_to_string o'.typ))


(** [typecheck_expression_expecting cenv venv vinit instanceof typ1 e] checks, using the
    environments [cenv] and [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the expression [e] has a type compatible with type [typ1]. *)
and typecheck_expression_expecting (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (typ1 : typ)
    (e : expression) : TMJ.expression =
  let e' = typecheck_expression cenv venv vinit instanceof e in
  if not (compatible Location.(type_tmj_to_lmj (startpos e) (endpos e) e'.typ) typ1 instanceof) then
    error e
      (sprintf "Type mismatch, expected %s, got %s" (type_to_string typ1) (tmj_type_to_string e'.typ));
  e'

(** [typecheck_expression cenv venv vinit instanceof e] checks, using the environments [cenv] and
    [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the expression [e] is well typed.
    If [typecheck_expression] succeeds, the type of [e] is returned. *)
and typecheck_expression (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (e : expression) : TMJ.expression =
  match Location.content e with
  | EConst (ConstBool b) -> 
      mke (TMJ.EConst (ConstBool b)) TypBool

  | EConst (ConstInt i) ->
      mke (TMJ.EConst (ConstInt i)) TypInt

  | EGetVar v ->
     let typ = vlookup v venv in
     let v' = Location.content v in
     if not (S.mem v' vinit) then
       error v (sprintf "Variable %s has not been initialized" v');
     mke (TMJ.EGetVar (Location.content v)) typ

  | EUnOp (op, e) ->
      let expected, returned =
        match op with
        | UOpNot -> TypBool, TypBool
      in
      let e' = typecheck_expression_expecting cenv venv vinit instanceof expected e in
      mke (TMJ.EUnOp (op, e')) returned

  | EBinOp (op, e1, e2) ->
      let expected, returned =
        match op with
        | OpAdd
        | OpSub
        | OpMul -> TypInt, TypInt
        | OpLt  -> TypInt, TypBool
        | OpAnd -> TypBool, TypBool
      in
      let e1' = typecheck_expression_expecting cenv venv vinit instanceof expected e1 in
      let e2' = typecheck_expression_expecting cenv venv vinit instanceof expected e2 in
      mke (TMJ.EBinOp (op, e1', e2')) returned

  | EMethodCall (o, callee, expressions) ->
      typecheck_call cenv venv vinit instanceof o callee expressions

  | EArrayGet (earray, eindex) ->
      let eindex' = typecheck_expression_expecting cenv venv vinit instanceof TypInt eindex in
      let earray' = typecheck_expression_expecting cenv venv vinit instanceof TypIntArray earray in
      mke (TMJ.EArrayGet (earray', eindex')) TypInt

  | EArrayAlloc elength ->
      let elength' = typecheck_expression_expecting cenv venv vinit instanceof TypInt elength in
      mke (TMJ.EArrayAlloc elength') TypIntArray

  | EArrayLength earray ->
      let earray' = typecheck_expression_expecting cenv venv vinit instanceof TypIntArray earray in
      mke (TMJ.EArrayLength earray') TypInt

  | EThis ->
     mke TMJ.EThis (vlookup (Location.make (Location.startpos e) (Location.endpos e) "this") venv)

  | EObjectAlloc id ->
      clookup id cenv |> ignore;
      mke (TMJ.EObjectAlloc (Location.content id)) (Typ id)

(** [typecheck_instruction cenv venv vinit instanceof inst] checks, using the environments [cenv] and
    [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the instruction [inst] is well typed.
    If [typecheck_instruction] succeeds, the new set of initialized variables is returned. *)
let rec typecheck_instruction (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (inst : instruction) : (TMJ.instruction * S.t) =
  match inst with
  | ISetVar (v, e) ->
      let vinit =
        S.add (Location.content v) vinit
      in
      let typ = vlookup v venv in
      let e' = typecheck_expression_expecting cenv venv vinit instanceof typ e in
      (TMJ.ISetVar (Location.content v, type_lmj_to_tmj typ, e'), vinit)

  | IArraySet (earray, eindex, evalue) ->
      typecheck_expression_expecting cenv venv vinit instanceof TypIntArray
        (Location.make (Location.startpos earray) (Location.endpos earray) (EGetVar earray))
      |> ignore;
      let eindex' = typecheck_expression_expecting cenv venv vinit instanceof TypInt eindex in
      let evalue' = typecheck_expression_expecting cenv venv vinit instanceof TypInt evalue in
      (TMJ.IArraySet (Location.content earray, eindex', evalue'), vinit)

  | IBlock instructions ->
      let instructions', vinit =
        List.fold_left
          (fun (acc, vinit) inst ->
          let inst, vinit = typecheck_instruction cenv venv vinit instanceof inst in
          (inst :: acc, vinit))
        ([], vinit)
        instructions
      in
      (TMJ.IBlock (List.rev instructions'), vinit)

  | IIf (cond, ithen, ielse) ->
      let cond' = typecheck_expression_expecting cenv venv vinit instanceof TypBool cond in
      let ithen', vinit1 =
        typecheck_instruction cenv venv vinit instanceof ithen
      in
      let ielse', vinit2 =
        typecheck_instruction cenv venv vinit instanceof ielse
      in
      (TMJ.IIf (cond', ithen', ielse'), S.inter vinit1 vinit2)

  | IWhile (cond, ibody) ->
      let cond' = typecheck_expression_expecting cenv venv vinit instanceof TypBool cond in
      let ibody', vinit = typecheck_instruction cenv venv vinit instanceof ibody in
      (TMJ.IWhile (cond', ibody'), vinit)

  | ISyso e ->
     let e' = typecheck_expression_expecting cenv venv vinit instanceof TypInt e in
     (TMJ.ISyso e', vinit)

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
    (m : metho) : TMJ.metho =

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
  let body', vinit =
    match typecheck_instruction cenv venv vinit instanceof (IBlock m.body) with 
    | IBlock body', vinit -> body', vinit 
    | _ -> assert false
  in
  let return' = typecheck_expression_expecting cenv venv vinit instanceof m.result m.return in
  TMJ.{
    formals = List.map (fun (id, typ) -> Location.content id, type_lmj_to_tmj typ) m.formals;
    result  = type_lmj_to_tmj m.result;
    locals  = List.map (fun (id, typ) -> Location.content id, type_lmj_to_tmj typ) m.locals;
    body    = body';
    return  = return'
  }

(** [typecheck_class cenv instanceof (name, c)] checks, using the environments [cenv] and [venv]
    and the [instanceof] function, that the class [name] with type [c] is well typed. *)
let typecheck_class (cenv : class_env) (instanceof : identifier -> identifier -> bool)
    ((name, c) : identifier * clas) : TMJ.identifier * TMJ.clas =
  let attribute_env, _ = clookup name cenv in
  let venv = SM.add "this" (Typ name) attribute_env in
  let methods' = 
    List.map (fun (id, metho) -> 
                (Location.content id, typecheck_method cenv venv instanceof metho)
              ) c.methods
  in
  (Location.content name, 
  TMJ.{
    extends    = (match c.extends with None -> None | Some id -> Some (Location.content id));
    attributes = List.map (fun (id, typ) -> Location.content id, type_lmj_to_tmj typ) c.attributes;
    methods    = methods';
  })

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

let typecheck_program (p : program) : TMJ.program =
  let cmap = class_map p.defs in
  let instanceof = create_instanceof cmap in
  let cenv =
    add_method cmap instanceof
    |> SM.map extract_class_type
  in
  let defs' = 
    List.map (typecheck_class cenv instanceof) p.defs
  in
  let venv = SM.singleton "this" (Typ p.name) in
  TMJ.{
    name = Location.content p.name;
    defs = defs';
    main_args = Location.content p.main_args;
    main      = fst (typecheck_instruction cenv venv S.empty instanceof p.main)
  }
  