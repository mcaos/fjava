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

let test_class_def_with_const test_ctx =
  let lexbuf =
    Lexing.from_string "class Hoge extends Object { Object x; Hoge(Object x) { super(); this.x = x; } }" in
  let parsed: Class.t list = Parser.toplevel Lexer.main lexbuf in
  let syntax: Class.t list = [
    {
      Class.name = Id.make "Hoge";
      super = Type.make "Object";
      fields = [ {
        Field.name = Id.make "x";
        ty = Type.make "Object";
      } ];
      constructor = {
        Constructor.name = Id.make "Hoge";
        params = [ (Id.make "x", Type.make "Object") ];
        super_args = [];
        body = [ (Id.make "x", Var (Id.make "x")) ];
      };
      methods = [];
    }
  ] in
  assert_equal syntax parsed

let test_class_def_pair test_ctx =
  let lexbuf =
    Lexing.from_string "class Pair extends Object { Object fst; Object snd; Pair(Object fst, Object snd) { super(); this.fst = fst; this.snd = snd; } Pair setfst(Object newfst) { return new Pair(newfst, this.snd); } }" in
  let parsed: Class.t list = Parser.toplevel Lexer.main lexbuf in
  let syntax: Class.t list = [
    {
      Class.name = Id.make "Pair";
      super = Type.make "Object";
      fields = [
        { Field.name = Id.make "fst"; ty = Type.make "Object"; };
        { Field.name = Id.make "snd"; ty = Type.make "Object"; }
      ];
      constructor = {
        Constructor.name = Id.make "Pair";
        params = [
          (Id.make "fst", Type.make "Object");
          (Id.make "snd", Type.make "Object")
        ];
        super_args = [];
        body = [
          (Id.make "fst", Var (Id.make "fst"));
          (Id.make "snd", Var (Id.make "snd"))
        ];
      };
      methods = [ {
        Method.name = Id.make "setfst";
        params = [ (Id.make "newfst", Type.make "Object") ];
        body = New (
          Id.make "Pair",
          [Var (Id.make "newfst"); FieldGet(Var (Id.make "this"), Id.make "snd") ]
        );
        return_type = Type.make "Pair"
      } ];
    }
  ] in
  assert_equal syntax parsed


let suite =
  "parser">::: [
    "test_simple_class_def">:: test_simple_class_def;
    "test_class_def_with_const">:: test_class_def_with_const;
    "test_class_def_pair">:: test_class_def_pair
  ]

let () = run_test_tt_main suite
