%{
%}

%token <string> ID

%token CLASS EXTENDS SUPER THIS RETURN NEW
%token COMMA PERIOD EQ LBRACE LPAREN RBRACE RPAREN SEMICOLON
%token EOF

%start toplevel
%type <string list> toplevel
%%

toplevel :
    class_def_list EOF { $1 }

class_def_list :
    class_def { [$1] }
  | class_def class_def_list { $1 :: $2 }

class_def :
    CLASS ID EXTENDS ID LBRACE
      field_list
      constructer
    RBRACE { $2 ^ $4 ^ String.concat " " $6 ^ $7}

field_list :
    { [] }
  | field_list field { $1 @ [$2] }

field:
  ID ID SEMICOLON { $1 ^ $2 }

constructer :
  ID LPAREN param_list_opt RPAREN LBRACE
    SUPER LPAREN argument_list RPAREN SEMICOLON
  RBRACE { $1 ^ (String.concat " " $3) ^ (String.concat " " $8) }

param_list_opt :
    { [] }
  | param_list { $1 }

param_list :
    param { [$1] }
  | param COMMA param_list { $1 :: $3 }

param :
  ID ID { $1 ^ $2 }

argument_list:
    { [] }
  | argument COMMA argument_list { $1 :: $3 }

argument :
  ID { $1 }
