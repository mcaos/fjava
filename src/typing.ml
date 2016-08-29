open Printf

open Syntax

module Environment = Map.Make (
  struct
    type t = Type.t
    let compare = compare
  end
)

exception Type_error of string

let base_class_name = "Object"
let base_class = {
  Class.name = Id.make base_class_name;
  super = "???";
  fields = [];
  constructor = {
    Constructor.name = Id.make base_class_name;
    params = [];
    body = [];
    super_args = [];
  };
  methods = [];
}


let get_class class_table name =
  if not (Environment.mem name class_table) then
    raise (Type_error (sprintf "%s is not found in classtable." name));
  Environment.find name class_table


let super_of class_table cls =
  get_class class_table (Class.super cls)


let rec is_subclass class_table c0 c1 =
  if c0 = c1 then
    (* C <: C *)
    true
  else if c1 = base_class_name then
    false
  else
    (* recursively check c1's super class is equal to c0 or not *)
    is_subclass class_table c0 (Class.super (get_class class_table c1))


let rec is_subclasses class_table l0 l1 =
  match l0, l1 with
  | [], [] -> true
  | [], _  -> false
  | _ , [] -> false
  | (c0 :: cs0), (c1 :: cs1) ->
      (is_subclass class_table c0 c1) && (is_subclasses class_table cs0 cs1)


let get_field cls field_name =
  try
    List.find (fun f -> Field.name f = field_name) (Class.fields cls)
  with
    Not_found ->
      raise (Type_error (sprintf "the field `%s` is not member of the class `%s`" field_name (Class.name cls)))


let get_method class_table cls method_name =
  let rec get_method class_table cls' method_name =
    try
      List.find (fun m -> Method.name m = method_name) (Class.methods cls')
    with Not_found ->
      (* find method in super class *)
      if Class.ty cls' = Class.ty base_class then
        raise (Type_error (sprintf "Method not found: %s" method_name));
      let super_class = super_of class_table cls' in
      get_method class_table super_class method_name in
  get_method class_table cls method_name



let rec check_expr class_table env = function
  (* return var type if exists in env *)
  | Var({ Id.name = n; }) when Environment.mem n env -> Environment.find n env
  | Var(n) -> raise (Type_error (sprintf "`%s` is not defined in current environment" (Id.name n)))
  | FieldGet(e, n) ->
    let c = get_class class_table (check_expr class_table env e) in
    let f = get_field c (Id.name n) in
    Field.ty f
  | MethodCall(instance, method_name, args) ->
    let args_type = List.map (check_expr class_table env) args in
    let class_of_inst = get_class class_table (check_expr class_table env instance) in
    let meth = get_method class_table class_of_inst (Id.name method_name) in
    let meth_params_type = Method.params_type meth in
    if is_subclasses class_table meth_params_type args_type then
      Method.return_type meth
    else
      raise (Type_error (sprintf "Invalid arguments: %s" (Id.name method_name)))
  | New(constructor_name, args) ->
    let args_type = List.map (check_expr class_table env) args in
    let cls = get_class class_table (Id.name constructor_name) in
    let params_type = Constructor.params_type (Class.constructor cls) in
    if is_subclasses class_table params_type args_type then
      Class.ty cls
    else
      raise (Type_error (sprintf "Invalid constructor arguments: %s" (Constructor.name (Class.constructor cls))))
  | Cast(target_id, expr) ->
    let subject_ty = check_expr class_table env expr in
    let target_ty = Type.make (Id.name target_id) in
    if is_subclass class_table target_ty subject_ty then
      (* up cast: subject is subclass of target *)
      target_ty
    else if is_subclass class_table subject_ty target_ty then
      (* down cast: target is subclass of subject *)
      target_ty
    else
      (* stupid cast *)
        begin
          eprintf
            "stupid cast from class %s to class '%s"
            subject_ty target_ty;
          target_ty
        end


let check_constructor_super class_table env cls =
  let constructor = Class.constructor cls in
  if Class.ty cls = Class.ty base_class then
    ()
  else
    (* class type not Object *)
    let super_class = super_of class_table cls in
    let super_args = Constructor.super_args constructor in
    let super_args_type = List.map (check_expr class_table env) super_args in
    let super_params_type = Constructor.params_type (Class.constructor super_class) in
    (* is super_args_type is subclass of super_params_type *)
    if is_subclasses class_table super_params_type super_args_type then
      ()
    else
      (* type mismatch OR invalid num of args *)
      raise (Type_error "Invalid arguments for super class constructor")


(* Class.t Environment.t -> Type.t Environment.t -> Constructor.t -> Id.t * expr -> unit *)
let check_field_initialization class_table env constructor = function
    (* check if this.hoge = hoge *)
    (field_name, Var param) when Id.name field_name = Id.name param ->
      (* check if field type equal to param type *)
      let class_name = Constructor.name constructor in
      let this = get_class class_table (Type.make class_name) in
      let field = get_field this (Id.name field_name) in
      let field_ty = Field.ty field in
      let param_ty = check_expr class_table env (Var param) in
      if field_ty <> param_ty then
        raise (Type_error (
          "cannot initialize field with different type: '" ^ field_ty ^ "' != '" ^ param_ty ^ "'"));
      ()
  | (field_name, _) ->
      raise (Type_error "Invalid field initialization")



let check_constructor class_table env cls =
  (* check if class name equeal to constructor name *)
  let constructor = Class.constructor cls in
  if Class.name cls <> Constructor.name constructor then
    raise (Type_error (sprintf "Invalid constructor name: %s" (Constructor.name constructor)));
  (* add params to env *)
  let env' = List.fold_left
    (fun e (name, ty) -> Environment.add (Id.name name) ty e) env (Constructor.params constructor) in
  List.iter
    (check_field_initialization class_table env' constructor)
    (Constructor.body constructor);
  check_constructor_super class_table env' cls


let check_method_oveerride class_table cls meth = ()


let check_method class_table env meth cls =
  (* add params to env *)
  let env' =
    List.fold_left
      (fun e (name, ty) -> Environment.add (Id.name name) ty e)
      env (Method.params meth) in
  let actual_type = check_expr class_table env' (Method.body meth) in
  if not (is_subclass class_table (Method.return_type meth) actual_type) then
    raise (Type_error (
      "Invalid method return type: " ^ (Method.name meth)));
  check_method_oveerride class_table cls meth


let check_methods class_table env cls =
  let methods = Class.methods cls in
  let env' = Environment.empty in
  ignore (List.fold_left begin fun env'' meth ->
    let name = Method.name meth in
    if Environment.mem name env' then
      raise (Type_error (
        sprintf "duplicate method '%s' in class '%s'" (Method.name meth) (Class.name cls)));
    check_method class_table env meth cls;
    Environment.add name meth env'
  end env' methods);
  ()


let check_super_class table cls =
  (* TODO *)
  ()

let list_compare_without_order l1 l2 =
  let sorted1 = List.sort compare l1 in
  let sorted2 = List.sort compare l2 in
  sorted1 = sorted2

let check_uninitialized_fields cls =
  let constructor_fields = Constructor.fields_name (Class.constructor cls) in
  let class_fields = List.map Field.name (Class.fields cls) in
  if list_compare_without_order constructor_fields class_fields then
    ()
  else
    raise (Type_error ("all fields must be initialized just enough in class: " ^ (Class.name cls)))


let check_class class_table env cls =
  check_super_class class_table cls;
  let env' = Environment.add "this" (Class.ty cls) env in
  (* check fields *)
  let env' = List.fold_left begin
    fun e field ->
      let field_name = Field.name field in
      let field_type = Field.ty field in
      (* check_field *)
      Environment.add field_name field_type e
  end env' (Class.fields cls) in
  check_uninitialized_fields cls;
  check_constructor class_table env' cls;
  check_methods class_table env' cls


let check_class_table class_table =
  let env = Environment.empty in
  Environment.iter (fun _ c -> check_class class_table env c) class_table


(* Class.t list -> Class.t Environment.t *)
let make_class_table classes =
  let table = Environment.singleton (Class.name base_class) base_class in
  List.fold_left begin fun tb cls ->
    let name = Class.name cls in
    if Environment.mem name tb then
      raise (Type_error ("duplicate class: " ^ name))
    else
      Environment.add name cls tb
  end table classes


let check program =
  let class_table = make_class_table program in
  check_class_table class_table
