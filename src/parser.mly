%{
open Syntax
%}

%token <string> ID

%token CLASS EXTENDS SUPER THIS RETURN NEW EQ
%token COMMA PERIOD LBRACE LPAREN RBRACE RPAREN SEMICOLON
%token EOF

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    class_def_list EOF { $1 }

class_def_list :
    class_def { [$1] }
  | class_def class_def_list { $1 :: $2 }

class_def :
  CLASS ID EXTENDS ID LBRACE
    field_list
    constructor
    method_def_list
  RBRACE {
    {
      Class.name = Id.make $2;
      super = Type.make $4;
      fields = $6;
      constructor = $7;
      methods = $8;
    }
  }

field_list :
    { [] }
  | field_list field { $1 @ [$2] }

field:
  ID ID SEMICOLON { { Field.name = Id.make $2; ty = Type.make $1 } }

constructor :
  ID LPAREN param_list_opt RPAREN LBRACE
    SUPER LPAREN argument_list RPAREN SEMICOLON
    field_init_list
  RBRACE { {
    Constructor.name = (Id.make $1);
    params = $3;
    super_args = $8;
    body = $11;
  } }

method_def_list :
    { [] }
  | method_def method_def_list { $1 :: $2 }

method_def :
  ID ID LPAREN param_list_opt RPAREN LBRACE
    RETURN expression SEMICOLON
  RBRACE {
    {
      Method.name = Id.make $2;
      params = $4;
      body = $8;
      return_type = Type.make $1;
    }
  }

param_list_opt :
    { [] }
  | param_list { $1 }

param_list :
    param { [$1] }
  | param COMMA param_list { $1 :: $3 }

param :
  ID ID { (Id.make $2, Type.make $1) }

argument_list:
    { [] }
  | argument COMMA argument_list { $1 :: $3 }

argument :
  expression { $1 }

field_init_list :
    { [] }
  | field_init SEMICOLON field_init_list { $1 :: $3 }

field_init :
  THIS PERIOD ID EQ ID { (Id.make $3, Var (Id.make $5)) }


expression :
  | var { $1 }
  | field_get { $1 }
  | method_call { $1 }
  | new_instance { $1 }
  | cast { $1 }

var :
    ID { Var (Id.make $1) }
  | THIS { Var (Id.make "this") }

field_get :
  expression PERIOD ID { FieldGet ($1, Id.make $3) }

method_call :
  expression PERIOD ID LPAREN argument_list RPAREN {
    MethodCall ($1, Id.make $3, $5)
  }

new_instance :
  NEW ID LPAREN argument_list RPAREN { New (Id.make $2, $4) }

cast :
  LPAREN ID RPAREN expression { Cast (Id.make $2, $4) }
