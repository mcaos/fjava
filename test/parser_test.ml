open OUnit2

open Syntax

let test_simple_class_def test_ctx =
  let lexbuf =
    Lexing.from_string "class A extends Object { A() { super(); } }" in
  let parsed: Class.t list = Parser.toplevel Lexer.main lexbuf in
  let syntax: Class.t list = [
    {
      Class.name = Id.make "A";
      super = Type.make "Object";
      fields = [];
      constructor = {
        Constructor.name = Id.make "A";
        params = [];
        super_args = [];
        body = [];
      };
      methods = [];
    }
  ] in
  assert_equal syntax parsed


let suite =
  "parser">::: [
    "test_simple_class_def">:: test_simple_class_def]

let () = run_test_tt_main suite
