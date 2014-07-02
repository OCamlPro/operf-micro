{
open Parser
}

let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let char = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' ]
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']


rule token = parse
  | newline+ | blank+ { token lexbuf }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | "["   { LBRACKET }
  | "]"   { RBRACKET }
  | ":"   { COLON }
  | ","   { COMMA }
  | char identchar* { IDENT (Lexing.lexeme lexbuf) }

  | float_literal
      { let s = Lexing.lexeme lexbuf in
        try FLOAT (float_of_string s)
        with Failure _ -> assert false (* shouldn't be possible *) }

  | "\""  { string_double_quote (Buffer.create 10) lexbuf }
  | "\'"  { string_quote (Buffer.create 10) lexbuf }

  | eof   { EOF }

and string_double_quote buf = parse
  | "\"" { STRING (Buffer.contents buf) }
  | [^'"']+
         { Buffer.add_string buf (Lexing.lexeme lexbuf);
           string_double_quote buf lexbuf }

and string_quote buf = parse
  | "\'" { STRING (Buffer.contents buf) }
  | [^'\'']+
         { Buffer.add_string buf (Lexing.lexeme lexbuf);
           string_quote buf lexbuf }

{

}
