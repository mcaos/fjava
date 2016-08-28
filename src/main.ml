open Printf

open Typing

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
    let program = Parser.toplevel Lexer.main lexbuf in
    check program;
  with e ->
    begin match e with
      Parser.Error ->
        let token = Lexing.lexeme lexbuf in
        let message = sprintf "parser error: unexpected token '%s'" token in
        print_error lexbuf message;
     | _ ->
        close_in_noerr ic;
        print_error lexbuf "unknown error";
        raise e
    end

let _ = read_from_file "../examples/test.java"
