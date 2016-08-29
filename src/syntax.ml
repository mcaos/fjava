module Id = struct
  type t = {
    name: string;
  }
  let make n = { name = n; }
  let name id = id.name
end

module Type = struct
  type t = string
  let make(x: t) : t = x
end

type expr =
    Var of Id.t
  | FieldGet of expr * Id.t
  | MethodCall of expr * Id.t * expr list
  | New of Id.t * expr list
  | Cast of Id.t * expr

module Method = struct
  type t = {
    name: Id.t;
    params: (Id.t * Type.t) list;
    body: expr;
    return_type: Type.t;
  }
  let name m = Id.name m.name
  let params m = m.params
  let body m = m.body
  let return_type m = m.return_type
end

module Constructor = struct
  type t = {
    name: Id.t;
    params: (Id.t * Type.t) list;
    body: (Id.t * expr) list;
    super_args: expr list;
  }
  let name c = Id.name c.name
  let params c = c.params
  let body c = c.body
  let super_args c = c.super_args

  let params_type c = List.map snd c.params
end

module Field = struct
  type t = {
    name: Id.t;
    ty: Type.t;
  }
  let name f = Id.name f.name
  let ty f = f.ty
end


module Class = struct
  type t = {
    name: Id.t;
    super: Type.t;
    fields: Field.t list;
    constructor: Constructor.t;
    methods: Method.t list;
  }
  let name c = Id.name c.name
  let super c = c.super
  let ty c = Type.make (Id.name c.name)
  let fields c = c.fields
  let constructor c = c.constructor
  let methods c = c.methods
end

type program = Class.t list
