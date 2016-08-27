open Printf

let print_error lexbuf message =
  let start = Lexing.lexeme_start_p lexbuf in
  let file = start.Lexing.pos_fname in
  let line = start.Lexing.pos_lnum in
  eprintf "%s:line %d: %s\n" file line message

let read_from_file file =
  print_endline ("Reading from " ^ file);
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  try
    let classes = Parser.toplevel Lexer.main lexbuf in
    print_string "hoge"
  with e ->
    begin match e with
      Parser.Error ->
        let token = Lexing.lexeme lexbuf in
        let message = sprintf "parser error: unexpected token '%s'" token in
        print_error lexbuf message;
        exit 1
    end

let _ = read_from_file "../examples/test.java"
