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
    CLASS ID EXTENDS ID LBRACE RBRACE { $2 ^ $4 }
