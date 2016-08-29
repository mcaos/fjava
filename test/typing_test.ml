open OUnit2

open Syntax
open Typing

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

let test_check_constructor_field_not_found test_ctx =
  let classes = [
    {
      Class.name = Id.make "A";
      super = "Object";
      fields = [ { Field.name = (Id.make "x"); ty = Type.make "Object" } ];
      constructor = {
        Constructor.name = Id.make "A";
        params = [ ( Id.make "y", Type.make "Object" ) ];
        body = [ (Id.make "y", Var (Id.make "y")) ];
        super_args = [];
      };
      methods = [];
    }
  ] in
  let class_table = make_class_table classes in
  assert_raises
    (Type_error "the field `y` is not member of the class `A`")
    (fun _ -> check_class_table class_table)

let suite =
  "typing">::: [
    "test_check_constructor">:: test_check_constructor;
    "test_check_constructor_init">:: test_check_constructor_init;
    "test_check_constructor_field_not_found">:: test_check_constructor_field_not_found
  ]

let () = run_test_tt_main suite
