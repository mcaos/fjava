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


let get_field cls field_name =
  try
    List.find (fun f -> Field.name f = field_name) (Class.fields cls)
  with
    Not_found ->
      raise (Type_error (sprintf "the field `%s` is not member of the class `%s`." field_name (Class.name cls)))


let rec check_expr class_table env = function
  (* return var type if exists in env *)
  | Var({ Id.name = n; }) when Environment.mem n env -> Environment.find n env
  | Var(n) -> raise (Type_error (sprintf "`%s` is not defined in current environment" (Id.name n)))
  | FieldGet(e, n) ->
    let c = get_class class_table (check_expr class_table env e) in
    let f = get_field c (Id.name n) in
    Field.ty f
  | _ -> raise (Type_error ("not impleented"))
  (*
  | MethodCall(e0, x0, es0) ->
    let ts0 = L.map (infer_expr ct env) es0 in
    let k0 = class_of ct (Type.name (infer_expr ct env e0)) in
    let m0 = choice_most_specific_method ct k0 x0 ts0 in
    Method.ret_type m0
  | New(x0, es0) ->
    let ts0 = L.map (infer_expr ct env) es0 in
    let k0 = class_of ct x0 in
    ignore (choice_most_specific_ctor ct k0 ts0);
    Type.name x0
  | Cast(t0, e0) ->
    let t1 = infer_expr ct env e0 in
    if not (is_subclass ct t0 t1) then
      raise (Type_error (sprintf "`%s` is not subclass of `%s`." (Type.name t1) (Type.name t0)));
    t0
  *)

let check_constructor class_table env constructor = ()


let check_method class_table env meth =
  (* add params to env *)
  let env' =
    List.fold_left
      (fun e (name, ty) -> Environment.add (Id.name name) ty e)
      env (Method.params meth) in
  ignore (check_expr class_table env' (Method.body meth) )
  (* TODO check match return type *)


let check_methods class_table env cls =
  let methods = Class.methods cls in
  let env' = Environment.empty in
  ignore (List.fold_left begin fun env'' meth ->
    let name = Method.name meth in
    if Environment.mem name env' then
      raise (Type_error (
        sprintf "duplicate method '%s' in class '%s'" (Method.name meth) (Class.name cls)));
    check_method class_table env meth;
    Environment.add name meth env'
  end env' methods);

  ()


let check_super_class table cls =
  (* TODO *)
  ()


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
  check_constructor class_table env' (Class.constructor cls);
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
