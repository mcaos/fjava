{
exception Lexer_error of string

let reservedWords = [
  ("class", Parser.CLASS);
  ("extends", Parser.EXTENDS);
  ("super", Parser.SUPER);
  ("this", Parser.THIS);
  ("return", Parser.RETURN);
  ("new", Parser.NEW);
  ("return", Parser.RETURN);
]
}

rule main = parse
    (* skip spacing and new line chars *)
    [' ' '\009' '\012' '\n']+ { main lexbuf }
  | "," { Parser.COMMA }
  | "." { Parser.PERIOD }
  | "=" { Parser.EQ }
  | "{" { Parser.LBRACE }
  | "(" { Parser.LPAREN }
  | "}" { Parser.RBRACE }
  | ")" { Parser.RPAREN }
  | ";" { Parser.SEMICOLON }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
    (* Lexing.lexeme lexbuf return match string *)
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
        _ -> Parser.ID id
    }
  | eof { Parser.EOF }
