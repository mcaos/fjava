open Syntax

let print_class cl =
  print_string (Class.name cl)

let check program =
  List.iter print_class program

