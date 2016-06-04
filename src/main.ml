let rec print_list = function
    [] -> ()
  | h::t -> print_endline h; print_list t

let read_from_file file =
  print_endline ("Reading from " ^ file);
  let ic = open_in file in
    try
      let lexbuf = Lexing.from_channel ic in
      let classes = Parser.toplevel Lexer.main lexbuf in
        print_list classes
    with e ->
      close_in_noerr ic;
      print_endline "Error";
      raise e


let _ = read_from_file "../examples/test.java"
