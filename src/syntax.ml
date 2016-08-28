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
end

module Constructor = struct
  type t = {
    name: Id.t;
    params: (Id.t * Type.t) list;
    body: (Id.t * expr) list;
    super_args: expr list;
  }
end

module Field = struct
  type t = {
    name: Id.t;
    ty: Type.t;
  }
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
end

type program = Class.t list
