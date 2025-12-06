%{
  open Mgoast

  exception Error

  let mk_expr e startpos endpos = 
    { edesc = e; eloc = (startpos, endpos) }
  let mk_expr_loc e loc = mk_expr e (fst loc) (snd loc)

  let mk_instr i startpos endpos = 
    { idesc = i; iloc = (startpos, endpos) }

  let mk_ident s startpos endpos = 
    { id = s; loc = (startpos, endpos) }

  (* Verif var avant := *)
  let ensure_idents exprs =
    try 
      List.map (fun e -> 
        match e.edesc with 
        | Var id -> id 
        | _ -> raise Exit
      ) exprs
    with Exit -> raise Error
%}

%token <int64> INT
%token <bool> BOOL
%token <string> IDENT
%token <string> STRING

%token TINT TBOOL TSTRING

%token PACKAGE IMPORT TYPE STRUCT FUNC VAR IF ELSE FOR RETURN NIL

%token LPAR RPAR LBRACKET RBRACKET
%token COMMA SEMI DOT
%token PLUS MINUS STAR DIV MOD
%token INC DEC
%token EQ NEQ LT LE GT GE
%token AND OR NOT
%token COLONEQ EQ_ASSIGN
%token EOF


%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left STAR DIV MOD
%nonassoc UMINUS NOT
%left DOT

%start fichier
%type <Mgoast.program> fichier

%%

fichier:
| PACKAGE main=IDENT SEMI f=option(import) decls=list(decl) EOF
    { match f with
      | None -> if main="main" then (false, decls) else raise Error
      | Some a -> if main="main" && Option.value a ~default:"" = "fmt" then (true, decls) else raise Error
    }
;

import:
| IMPORT id=IDENT SEMI { Some id }
;

decl:
| f=fonction {f}
| s=structure {s}
;

structure:
| TYPE id=ident STRUCT LBRACKET vs=separated_list(SEMI, vars) v=option(vars) RBRACKET SEMI
    {
      Struct {
        sname = id; 
        fields = match v with
          | None -> vs
          | Some a -> vs @ [Option.value a]
      } 
    }
;

fonction:
| FUNC id=ident LPAR params=separated_list(COMMA, vars) p=option(vars) RPAR 
  rets=option(type_retour) block=block SEMI
    { 
      Fun {
        fname = id; 
        params = 
          match p with 
          | None -> List.flatten params
          | Some v -> List.flatten params @ [Option.value v]
        return = Option.value rests ~default:[]; 
        body = block 
      }
    }
;

vars:
| ids=separated_nonempty_list(COMMA, ident) t=typ { List.map (fun id -> (id, t)) ids }
;

type_retour:
| tr=typ { [tr] }
| LPAR tr=separated_list(COMMA, typ) option(COMMA) RPAR { tr }
;

ident:
| id=IDENT { mk_ident id $startpos $endpos }
;

/* === Types === */
typ:
| TINT    { TInt }
| TBOOL   { TBool }
| TSTRING { TString }
| STAR id=IDENT { TStruct(id) }
;


/* === Instructions === */
block:
| LBRACKET is=separated_list(COMMA, instr) i2=option(instr) option(COMMA) RBRACKET
  { 
    let rec aux acc = function
      | [] -> List.rev acc
      | i :: rest ->
        match i.idesc with
        | Vars(_, _, seq) -> aux acc (seq @ rest)
        | _ -> aux (i :: acc) rest
    in
    aux [] (match i2 with Some i -> is @ [i] | None -> is)
  }
;


instr:
| s=instr_simple { s }
| b=block { mk_instr (Block b) $startpos $endpos }
| i=instr_if { i }
| a=autre_instr { a }
;

autre_instr:
| FOR b=block 
    { mk_instr (For(mk_expr (Bool true) $startpos $endpos, b)) $startpos $endpos }
| FOR c=expr b=block 
    { mk_instr (For(c, b)) $startpos $endpos }
| FOR init=option(instr_simple) SEMI cond=expr SEMI post=option(instr_simple) b=block
    { 
      let loop_body = b @ (match post with Some p -> [Option.value p] | None -> []) in
      let loop = mk_instr (For(cond, loop_body)) $startpos $endpos in
      let seq = (match init with Some i -> [Option.value i] | None -> []) @ [loop] in
      mk_instr (Block seq) $startpos $endpos
    }
| RETURN es=separated_list(COMMA, expr) { mk_instr (Return es) $startpos $endpos }
| VAR ids=separated_nonempty_list(COMMA, ident) t=option(typ) es=option(assign_instr)
    { mk_instr 
      (Vars(
          ids,
          Some t,
          [
            mk_instr 
            (Set(
              List.map (fun id -> mk_expr_loc (Var id) id.loc) ids, 
              match es with Some e -> Option.value e | None -> []
              )
            ) 
            $startpos $endpos
          ]
          )
      ) 
      $startpos $endpos }

assign_instr:
| EQ_ASSIGN es=separated_nonempty_list(COMMA, expr) { es }

instr_simple:
| l=separated_nonempty_list(COMMA, expr) EQ_ASSIGN r=separated_nonempty_list(COMMA, expr) { mk_instr (Set(l, r)) $startpos $endpos }
| l=separated_nonempty_list(COMMA, expr) COLONEQ r=separated_nonempty_list(COMMA, expr)
    { 
      let ids = ensure_idents l in
      mk_instr (Vars(ids, None, [mk_instr (Set(l, r)) $startpos $endpos])) $startpos $endpos 
    }
| e=expr INC { mk_instr (Inc e) $startpos $endpos }
| e=expr DEC { mk_instr (Dec e) $startpos $endpos }
| e=expr { mk_instr (Expr e) $startpos $endpos }
;

instr_if:
| IF c=expr b1=block 
    { mk_instr (If(c, b1, [])) $startpos $endpos }
| IF c=expr b1=block ELSE b2=block 
    { 
       mk_instr (If(c, b1, b2)) $startpos $endpos
    }
| IF c=expr b1=block ELSE b2=instr_if
    { 
       match b2.idesc with
       | Block b -> mk_instr (If(c, b1, b)) $startpos $endpos
       | If _ -> mk_instr (If(c, b1, [b2])) $startpos $endpos
       | _ -> raise Error 
    }
;

/* === Expressions === */
expr:
| n=INT         { Int(n) }
| b=BOOL        { Bool(b) }
| s=STRING      { String(s) }
| NIL           { Nil }
| LPAR e=expr RPAR { e }
| id=ident      { Var(id) }
| e=expr DOT id=ident { Dot(e, id) }
| e=expr DOT id=ident LPAR es=separated_list(COMMA, expr) RPAR 
    { 
      match e.edesc with
      | Var v when v.id = "fmt" && id.id = "Print" -> Print(es)
      | _ -> raise Error
    }
| NOT e=expr                { Unop(Not, e) }
| MINUS e=expr %prec UMINUS { Unop(Opp, e) }
| e1=expr operation=op e2=expr { Binop(operation, e1, e2) }
;

%inline op:
| PLUS  { Add }
| MINUS { Sub }
| STAR  { Mul }
| DIV   { Div }
| MOD   { Rem }
| EQ    { Eq  }
| NEQ   { Neq }
| LT    { Lt  }
| LE    { Le  }
| GT    { Gt  }
| GE    { Ge  }
| AND   { And }
| OR    { Or  }
;