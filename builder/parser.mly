%{
open Files
%}

%token <string> STRING IDENT
%token <float> FLOAT
%token COMMA COLON LBRACKET RBRACKET LBRACE RBRACE EOF

%start file
%type <Files.file> file
%%

file:
    | expr EOF { $1 }

expr:
    | LBRACE dict RBRACE
      { Dict $2 }
    | LBRACKET list RBRACKET
      { List $2 }
    | STRING
      { String $1 }
    | FLOAT
      { Float $1 }

dict:
    |
      { [] }
    | dict_field
      { [$1] }
    | dict_field COMMA dict
      { $1 :: $3 }

dict_field:
    | IDENT COLON expr
      { $1, $3 }

list:
    |
      { [] }
    | expr
      { [$1] }
    | expr COMMA list
      { $1 :: $3 }

%%
