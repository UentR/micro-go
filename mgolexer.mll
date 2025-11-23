{
  open Lexing
  open Mgoparser

  exception Error of string

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "package", PACKAGE;
        "import",  IMPORT;
        "type",    TYPE;      
        "struct",  STRUCT;
        "func",    FUNC;
        "var",     VAR;
        "if",      IF;
        "else",    ELSE;
        "for",     FOR;
        "return",  RETURN;
        "new",     NEW;
        "nil",     NIL;
        "true",    BOOL(true);
        "false",   BOOL(false);
        "int",     TINT;
        "bool",    TBOOL;
        "string",  TSTRING;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
  let buf = Buffer.create 1024

  let last_token = ref EOF

  (* Fonction wrapper pour mémoriser le token avant de le renvoyer *)
  let return_token t =
    last_token := t;
    t
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let hex   = ['0'-'9' 'a'-'f' 'A'-'F']
let ident = alpha (alpha | digit)*
let number = digit+ | ("0x" | "0X") hex+

rule token = parse
  | ['\n']            { new_line lexbuf; match !last_token with
                        | IDENT _ | INT _ | BOOL _ | STRING _ | NIL | RETURN | INC | DEC | RPAR | RBRACKET ->
                        last_token := SEMI; (* Important : on ne veut pas réinsérer un ; au prochain \n *)
                        SEMI
                        | _ -> token lexbuf  (* On ignore le \n *)
                      }
  | [' ' '\t' '\r']+  { token lexbuf }
  | "//" [^ '\n']* { token lexbuf }
  | "/*"              { comment lexbuf; token lexbuf }
  | '"'               { Buffer.clear buf; string lexbuf; STRING (Buffer.contents buf) }
  
  | number as n       { try return_token (INT(Int64.of_string n)) with _ -> raise (Error "literal constant too large") }
  | ident as id       { return_token (keyword_or_ident id) }

  | ";"  { return_token SEMI }
  | "("  { return_token LPAR }     
  | ")"  { return_token RPAR }
  | "{"  { return_token LBRACKET }    
  | "}"  { return_token RBRACKET }
  | ","  { return_token COMMA }    
  | "."  { return_token DOT }

  | "+"   { return_token PLUS } 
  | "-"   { return_token MINUS } 
  | "*"   { return_token STAR } 
  | "/"   { DIV } 
  | "%"   { MOD }
  | "++"  { INC }  
  | "--"  { DEC }
  | "=="  { EQ }   
  | "!="  { NEQ }   
  | "<"   { LT }   
  | "<="  { LE }  
  | ">"   { GT } 
  | ">="  { GE }
  | "&&"  { AND }  
  | "||"  { OR }    
  | "!"   { NOT }
  | ":="  { COLONEQ } 
  | "=" { EQ_ASSIGN }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { return_token EOF }

and comment = parse
  | "*/" { () }
  | '\n' { new_line lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }

and string = parse
  | '"'  { () }
  | '\\' 'n'  { Buffer.add_char buf '\n'; string lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; string lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; string lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; string lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; string lexbuf }
  | _     { raise (Error "illegal string character") }
  | eof   { raise (Error "unterminated string") }