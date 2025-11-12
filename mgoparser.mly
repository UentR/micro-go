%{

  open Lexing
  open Mgoast

  exception Error

%}

%token <int64> INT
%token <string> IDENT
%token <string> STRING
%token PACKAGE IMPORT TYPE STRUCT
%token LPAR RPAR BEGIN END SEMI STAR
%token COLON COMMA DOT
%token NOT 
%token MINUS PLUS DIV
%token LT GT 
%token ASSIGN
%token MOD AND OR
%token EOF

%start prog
%type <Mgoast.program> prog

%%

ident_list:
  x=ident                     { [x] }
| x=ident COMMA xs=ident_list { x :: xs }

varstyp:
  xs=ident_list t=mgotype { List.map (fun x -> (x, t)) xs }


prog:
| PACKAGE main=IDENT SEMI decls=list(decl) EOF
    { if main="main" then (false, decls) else raise Error}
| PACKAGE main=IDENT SEMI IMPORT fmt=STRING SEMI decls=list(decl) EOF
    { if main="main" && fmt="fmt" then (true, decls) else raise Error} 
;

ident:
  id = IDENT { { loc = $startpos, $endpos; id = id } }
;

decl:
  TYPE id=ident STRUCT BEGIN fl=loption(fields) END SEMI
  { Struct { sname = id; fields = List.flatten fl; } }
;

mgotype:
  id=IDENT
    { match id with
      | "int" -> TInt | "bool" -> TBool | "string" -> TString
      | _ -> raise Error }
| STAR s=IDENT { TStruct s }
;

fields:
  vts=varstyp SEMI fs=fields { vts @ fs }
| vts=varstyp SEMI?          { vts }
;

expr:
| e = expr_desc {  { eloc = ($startpos, $endpos); edesc = e } }
;

expr_desc:
| n=INT { Int(n) }
;
