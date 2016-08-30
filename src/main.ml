open Printf

open Typing

let print_error lexbuf message =
  let start = Lexing.lexeme_start_p lexbuf in
  let file = start.Lexing.pos_fname in
  let line = start.Lexing.pos_lnum in
  eprintf "%s:line %d: %s\n" file line message

let read_from_file file =
  print_endline ("Reading from: " ^ file);
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  try
    let program = Parser.toplevel Lexer.main lexbuf in
    check program;
    print_endline ("Check OK😇")
  with e ->
    begin match e with
      Parser.Error ->
        let token = Lexing.lexeme lexbuf in
        let message = sprintf "parser error: unexpected token '%s'" token in
        print_error lexbuf message;
        exit 1
    | Type_error(message) ->
        let message = sprintf "type error: %s" message in
        print_error lexbuf message;
        exit 1
     | _ ->
        close_in_noerr ic;
        print_error lexbuf "unknown error";
        raise e
    end

let _ =
  let len = Array.length Sys.argv in
  if len = 2 then
    let argfile = Sys.argv.(1) in
    read_from_file argfile
  else
    print_string "Usage: fjava <porgram>"

