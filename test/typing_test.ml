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

let suite =
  "typing">::: [
    "test_check_constructor">:: test_check_constructor
  ]

let () = run_test_tt_main suite
