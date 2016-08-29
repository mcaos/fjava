open OUnit2

open Syntax
open Typing


let test_check_uninitialized_field test_ctx =
  let classes1 = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [ { Field.name = Id.make "x"; ty = Type.make "Object" } ];
      constructor = {
        Constructor.name = Id.make "A";
        params = [];
        body = [];
        super_args = [];
      };
      methods = [];
    }
  ] in
  let classes2 = [
    {
      Class.name = Id.make "B";
      super = "Object";
      fields = [ { Field.name = Id.make "x"; ty = Type.make "Object" } ];
      constructor = {
        Constructor.name = Id.make "B";
        params = [ (Id.make "x", Type.make "Object"); (Id.make "y", Type.make "Object") ];
        body = [ (Id.make "x", Var (Id.make "x")); (Id.make "y", Var (Id.make "y")) ];
        super_args = [];
      };
      methods = [];
    }
  ] in
  let class_table1 = make_class_table classes1 in
  let class_table2 = make_class_table classes2 in
  begin
    assert_raises
      (Type_error "all fields must be initialized just enough in class: A")
      (fun _ -> check_class_table class_table1);
    assert_raises
      (Type_error "all fields must be initialized just enough in class: B")
      (fun _ -> check_class_table class_table2)
  end

let test_check_constructor test_ctx =
  let constructor_name_not_match_classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = Id.make "B";
        params = [];
        body = [];
        super_args = [];
      };
      methods = [];
    }
  ] in
  let class_table = make_class_table constructor_name_not_match_classes in
  assert_raises
    (Type_error "Invalid constructor name: B")
    (fun _ -> check_class_table class_table)

let test_check_constructor_init test_ctx =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [ { Field.name = (Id.make "x"); ty = Type.make "Object" } ];
      constructor = {
        Constructor.name = Id.make "A";
        params = [ ( Id.make "x", Type.make "int" ) ];
        body = [ (Id.make "x", Var (Id.make "x")) ];
        super_args = [];
      };
      methods = [];
    }
  ] in
  let class_table = make_class_table classes in
  assert_raises
    (Type_error "cannot initialize field with different type: 'Object' != 'int'")
    (fun _ -> check_class_table class_table)

let test_check_super_constructor text_ctx =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [];
      constructor = {
        Constructor.name = Id.make "A";
        params = [ (Id.make "x", Type.make "Object"); (Id.make "y", Type.make "A") ];
        body = [];
        super_args = [];
      };
      methods = [];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "B";
        params = [ (Id.make "x", Type.make "Object"); (Id.make "y", Type.make "Object") ];
        body = [];
        super_args = [Var(Id.make "x"); Var(Id.make "y") ];
      };
      methods = [];
    }
  ] in
  let class_table = make_class_table classes in
  assert_raises
    (Type_error "Invalid arguments for super class constructor")
    (fun _ -> check_class_table class_table)


let test_check_method_return_type test_ctx =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [ { Field.name = Id.make "x"; ty = Type.make "Object" } ];
      constructor = {
        Constructor.name = Id.make "A";
        params = [ (Id.make "x", Type.make "Object") ];
        body = [ (Id.make "x", Var (Id.make "x")) ];
        super_args = [];
      };
      methods = [ {
        Method.name = (Id.make "getX");
        params = [];
        body = FieldGet (Var(Id.make "this"), Id.make "x");
        return_type = Type.make "A";
      } ];
    }
  ] in
  let class_table = make_class_table classes in
  assert_raises
    (Type_error "Invalid method return type: getX")
    (fun _ -> check_class_table class_table)


let test_check_method_call test_ctx =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [ { Field.name = Id.make "x"; ty = Type.make "Object" } ];
      constructor = {
        Constructor.name = Id.make "A";
        params = [ (Id.make "x", Type.make "Object") ];
        body = [ (Id.make "x", Var (Id.make "x")) ];
        super_args = [];
      };
      methods = [ {
        Method.name = (Id.make "getX");
        params = [];
        body = FieldGet (Var(Id.make "this"), Id.make "x");
        return_type = Type.make "Object";
      } ];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [ { Field.name = Id.make "y"; ty = Type.make "Object" } ];
      constructor = {
        Constructor.name = Id.make "B";
        params = [ (Id.make "x", Type.make "Object"); (Id.make "y", Type.make "Object") ];
        body = [ (Id.make "y", Var (Id.make "y")) ];
        super_args = [Var(Id.make "x") ];
      };
      methods = [ {
        Method.name = (Id.make "get");
        params = [];
        body = MethodCall (Var(Id.make "this"), Id.make "getY", []);
        return_type = Type.make "Object";
      } ];
    }
  ] in
  let class_table = make_class_table classes in
  assert_raises
    (Type_error "Method not found: getY")
    (fun _ -> check_class_table class_table)

let test_check_invalid_args_method_call test_ctx =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [ { Field.name = Id.make "x"; ty = Type.make "Object" } ];
      constructor = {
        Constructor.name = Id.make "A";
        params = [ (Id.make "x", Type.make "Object") ];
        body = [ (Id.make "x", Var (Id.make "x")) ];
        super_args = [];
      };
      methods = [ {
        Method.name = (Id.make "getX");
        params = [];
        body = FieldGet (Var(Id.make "this"), Id.make "x");
        return_type = Type.make "Object";
      } ];
    };
    {
      Class.name = Id.make "B";
      super = "A";
      fields = [];
      constructor = {
        Constructor.name = Id.make "B";
        params = [ (Id.make "x", Type.make "Object") ];
        body = [];
        super_args = [Var(Id.make "x") ];
      };
      methods = [ {
        Method.name = (Id.make "get");
        params = [ (Id.make "y", Type.make "Object") ];
        body = MethodCall (Var(Id.make "this"), Id.make "getX", [Var(Id.make "y")]);
        return_type = Type.make "Object";
      } ];
    }
  ] in
  let class_table = make_class_table classes in
  assert_raises
    (Type_error "Invalid arguments: getX")
    (fun _ -> check_class_table class_table)


let suite =
  "typing">::: [
    "test_check_uninitialized_field">:: test_check_uninitialized_field;
    "test_check_constructor">:: test_check_constructor;
    "test_check_constructor_init">:: test_check_constructor_init;
    "test_check_super_constructor">:: test_check_super_constructor;
    "test_check_method_return_type">:: test_check_method_return_type;
    "test_check_method_call">:: test_check_method_call;
    "test_check_invalid_args_method_call">:: test_check_invalid_args_method_call
  ]

let () = run_test_tt_main suite
