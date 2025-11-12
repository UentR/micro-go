{

  open Lexing
  open Mgoparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "package",    PACKAGE;
      "import",     IMPORT;
      "type",       TYPE;      
      "struct",     STRUCT;
      "func",       FUNC;      
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let fmt = "fmt" 
let print = fmt DOT "Print"
let true = "true"
let false = "false"
let nil = "nil"
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "/*"              { comment lexbuf; token lexbuf }

  | '"' fmt '"'       { STRING("fmt") }

  | number as n  { try INT(Int64.of_string n) 
                   with _ -> raise (Error "literal constant too large") }
  | ident as id  { keyword_or_ident id }

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "*"  { STAR }
  | ":"  { COLON }
  | ","  { COMMA }
  | "."  { DOT }
  | "!"  { NOT }
  | "-"  { MINUS }
  | "+"  { PLUS }
  | "/"  { DIV }
  | "<"  { LT }
  | ">"  { GT }
  | "%"  { MOD }
  | "&" { AND }
  | "|" { OR }
  | "="  { ASSIGN }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | '\n' { new_line lexbuf; comment lexbuf }
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }

and new_line = parse
  | '\n' { new_line lexbuf }
  | _    { () }

and hexa = parse
  | ['0'-'9' 'a'-'f' 'A'-'F'] { lexeme lexbuf }

(* Les constantes entières doivent etre comprises entre -2^63 et 2^63-1 *)
and entier = parse
  | digit+ { lexeme lexbuf }
  | ["0x" "0X"] hexa+ { lexeme lexbuf }

and car = parse
  | ["\n" "\t" "\"" "\\" ] { lexeme lexbuf }
  | [ '\032'-'\033' '\035'-'\091' '\093'-'\126' ] { lexeme lexbuf }

and chaine = parse
  | '"' char* '"' { lexeme lexbuf }

and fichier = parse
  | PACKAGE main SEMI (IMPORT fmt SEMI)? decl* EOF

and decl = parse
  | structure 
  | fonction

and structure = parse
  | TYPE ident STRUCT BEGIN (vars SEMI)* (var SEMI)? END SEMI

and fonction = parse
  | FUNC ident LPAR (vars COMMA)* vars? RPAR type_retour bloc

and vars = parse
  | ident (COMMA ident)* type

and type_retour = parse
  | type
  | LPAR type (COMMA type)* COMMA? RPAR

and type = parse
  | int
  | bool
  | string
  | STAR ident

and expr = parse
  | [entier chaine true false nil]
  | LPAR expr RPAR
  | ident
  | expr DOT ident
  | ident LPAR (expr COMMA)* expr? RPAR
  | print LPAR (expr COMMA)* expr? RPAR
  | [(NOT expr) (MINUS expr)] 
  | expr op expr

and op = parse
  | [(ASSIGN ASSIGN) (NOT ASSIGN) LT (LT ASSIGN) GT (GT ASSIGN)]
  | [PLUS MINUS STAR DIV MOD (AND AND) (OR OR)]

and bloc = parse
  | BEGIN (instruction SEMI)* instruction? END

and instr = parse
  | [instr_simple bloc instr_if]
  | "var" ident (COMMA ident)* type? (ASSIGN expr (COMMA expr)*)?
  | "return" (expr COMMA)* expr?
  | "for" bloc
  | "for" expr bloc
  | "for" instr_simple? SEMI expr SEMI instr_simple? bloc

and instr_simple = parse
  | expr
  | expr [(PLUS PLUS) (MINUS MINUS)]
  | expr (COMMA expr)* ASSIGN expr (COMMA expr)*
  | ident (COMMA ident)* COLON ASSIGN expr (COMMA expr)*

and instr_if = parse
  | "if" expr bloc
  | "if" expr bloc "else" bloc
  | "if" expr bloc "else" instr_if
