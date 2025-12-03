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

  (* Auto ; *)
  let last_token = ref EOF
  let next_token = ref None (* cas } sans \n*)
  let mem_token t =
    last_token := t;
    t
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let hex   = ['0'-'9' 'a'-'f' 'A'-'F']
let ident = alpha (alpha | digit)*
let number = digit+ | ("0x" | "0X") hex+

rule real_token = parse
  | ['\n']            { new_line lexbuf; match !last_token with
                        | IDENT _ | INT _ | BOOL _ | STRING _ | NIL | RETURN | INC | DEC | RPAR | RBRACKET | TINT | TBOOL | TSTRING ->
                        last_token := SEMI; (* sinon ;\n;\n;\n... *)
                        SEMI
                        | _ -> real_token lexbuf
                      }
  | "}"  { 
      match !last_token with
      | IDENT _ | INT _ | BOOL _ | STRING _ | NIL | RETURN | INC | DEC |
        RPAR | RBRACKET | TINT | TBOOL | TSTRING ->
          next_token := Some RBRACKET;
          last_token := SEMI;
          SEMI
      | _ -> 
          mem_token RBRACKET 
    }

  | [' ' '\t' '\r']+  { real_token lexbuf }
  | "//" [^ '\n']*    { real_token lexbuf }
  | "/*"              { comment lexbuf; real_token lexbuf }
  | '"'               { Buffer.clear buf; string lexbuf; mem_token (STRING (Buffer.contents buf)) }
  
  | number as n       { try mem_token (INT(Int64.of_string n)) with _ -> raise (Error "literal constant too large") }
  | ident as id       { mem_token (keyword_or_ident id) }

  | ";"  { mem_token SEMI }
  | "("  { mem_token LPAR }     
  | ")"  { mem_token RPAR }
  | "{"  { mem_token LBRACKET }    
  | ","  { mem_token COMMA }    
  | "."  { mem_token DOT }

  | "+"   { mem_token PLUS } 
  | "-"   { mem_token MINUS } 
  | "*"   { mem_token STAR } 
  | "/"   { mem_token DIV } 
  | "%"   { mem_token MOD }
  | "++"  { mem_token INC }  
  | "--"  { mem_token DEC }
  | "=="  { mem_token EQ }   
  | "!="  { mem_token NEQ }   
  | "<"   { mem_token LT }   
  | "<="  { mem_token LE }  
  | ">"   { mem_token GT } 
  | ">="  { mem_token GE }
  | "&&"  { mem_token AND }  
  | "||"  { mem_token OR }    
  | "!"   { mem_token NOT }
  | ":="  { mem_token COLONEQ } 
  | "="   { mem_token EQ_ASSIGN }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { mem_token EOF }

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

{
  let token lexbuf =
    match !next_token with
    | Some t -> 
        next_token := None;
        mem_token t
    | None -> 
        real_token lexbuf
}