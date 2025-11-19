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

  (* Fonction utilitaire pour vérifier que la partie gauche d'un := est bien composée de variables *)
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

/* Tokens de Types */
%token TINT TBOOL TSTRING

/* Mots-clés */
%token PACKAGE IMPORT TYPE STRUCT FUNC VAR IF ELSE FOR RETURN NEW NIL

/* Ponctuation et Opérateurs */
%token LPAR RPAR LBRACKET RBRACKET
%token COMMA SEMI DOT
%token PLUS MINUS STAR DIV MOD
%token INC DEC
%token EQ NEQ LT LE GT GE
%token AND OR NOT
%token COLONEQ EQ_ASSIGN
%token EOF

/* === PRIORITÉS (Ordre croissant) === */
%nonassoc NO_ELSE
%nonassoc ELSE
%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left STAR DIV MOD
%nonassoc UMINUS NOT
%left DOT /* Priorité maximale pour résoudre le conflit expr.id */

%start prog
%type <Mgoast.program> prog

%%

/* === Programme Principal === */
prog:
| PACKAGE main=IDENT SEMI decls=list(decl) EOF
    { if main="main" then (false, decls) else raise Error }
| PACKAGE main=IDENT SEMI IMPORT fmt=STRING SEMI decls=list(decl) EOF
    { if main="main" && fmt="fmt" then (true, decls) else raise Error } 
;

/* === Déclarations (Top Level) === */
decl:
| TYPE id=ident STRUCT LBRACKET fl=list(field_decl) RBRACKET SEMI
    { Struct { sname = id; fields = List.flatten fl } }
| FUNC id=ident LPAR params=separated_list(COMMA, param_decl) RPAR 
  rets=return_types block=block SEMI
    { Fun { fname = id; params = List.flatten params; return = rets; body = block } }
;

field_decl:
| ids=separated_nonempty_list(COMMA, ident) t=typ SEMI { List.map (fun id -> (id, t)) ids }
;

param_decl:
| ids=separated_nonempty_list(COMMA, ident) t=typ { List.map (fun id -> (id, t)) ids }
;

return_types:
| /* empty */ { [] }
| t=typ { [t] }
| LPAR ts=separated_list(COMMA, typ) RPAR { ts }
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
| LBRACKET s=instr_list RBRACKET { s }
;

instr_list:
| /* empty */ { [] }
| i=instr SEMI rest=instr_list 
    { 
      match i.idesc with
      | Vars(ids, t, []) -> [{ i with idesc = Vars(ids, t, rest) }] 
      | _ -> i :: rest 
    }
;

/* Séparation des instructions en "Simple" et "Complexe" 
   pour résoudre les conflits dans la boucle FOR et l'affectation 
*/

instr:
| s=simple_stmt { s }
| s=compound_stmt { s }
;

simple_stmt:
/* Affectation : x, y = e1, e2 */
| l=separated_nonempty_list(COMMA, expr) EQ_ASSIGN r=separated_nonempty_list(COMMA, expr)
    { mk_instr (Set(l, r)) $startpos $endpos }

/* Déclaration courte : x, y := e1, e2 
   On parse 'expr' à gauche pour éviter le conflit avec '=', puis on vérifie que ce sont des variables */
| l=separated_nonempty_list(COMMA, expr) COLONEQ r=separated_nonempty_list(COMMA, expr)
    { 
      let ids = ensure_idents l in
      mk_instr (Vars(ids, None, [mk_instr (Set(l, r)) $startpos $endpos])) $startpos $endpos 
    }

/* Incrément / Décrément */
| e=expr INC { mk_instr (Inc e) $startpos $endpos }
| e=expr DEC { mk_instr (Dec e) $startpos $endpos }

/* Expression seule (appel de fonction) */
| e=expr { mk_instr (Expr e) $startpos $endpos }

/* Cas spécial fmt.Print qui est une "expression" primitive mais instruction */
| fmt_str=IDENT DOT print_str=IDENT LPAR es=separated_list(COMMA, expr) RPAR
    { 
       if fmt_str = "fmt" && print_str = "Print" then
         let e = mk_expr (Print es) $startpos $endpos in
         mk_instr (Expr e) $startpos $endpos 
       else
         let fmt_id = mk_ident fmt_str $startpos(fmt_str) $endpos(fmt_str) in
         let print_id = mk_ident print_str $startpos(print_str) $endpos(print_str) in
         let e_fmt = mk_expr_loc (Var fmt_id) fmt_id.loc in
         let e_dot = mk_expr (Dot(e_fmt, print_id)) $startpos $endpos in
         mk_instr (Expr e_dot) $startpos $endpos
    }
;

compound_stmt:
| b=block { mk_instr (Block b) $startpos $endpos }

/* IF / ELSE avec priorités pour le Dangling Else */
| IF c=expr b1=block %prec NO_ELSE 
    { mk_instr (If(c, b1, [])) $startpos $endpos }
| IF c=expr b1=block ELSE b2=compound_stmt_or_block
    { 
       match b2.idesc with
       | Block b -> mk_instr (If(c, b1, b)) $startpos $endpos (* else { ... } -> If(..., b) *)
       | If _ -> mk_instr (If(c, b1, [b2])) $startpos $endpos (* else if ... -> If(..., [If...]) *)
       | _ -> raise Error 
    }

/* Boucles FOR */
| FOR b=block 
    { mk_instr (For(mk_expr (Bool true) $startpos $endpos, b)) $startpos $endpos }
| FOR c=expr b=block 
    { mk_instr (For(c, b)) $startpos $endpos }
| FOR init=simple_stmt_opt SEMI cond=expr SEMI post=simple_stmt_opt b=block
    { 
      let loop_body = b @ (match post with Some p -> [p] | None -> []) in
      let loop = mk_instr (For(cond, loop_body)) $startpos $endpos in
      let seq = (match init with Some i -> [i] | None -> []) @ [loop] in
      mk_instr (Block seq) $startpos $endpos
    }

| RETURN es=separated_list(COMMA, expr) { mk_instr (Return es) $startpos $endpos }

| VAR ids=separated_nonempty_list(COMMA, ident) t=typ
    { mk_instr (Vars(ids, Some t, [])) $startpos $endpos }
| VAR ids=separated_nonempty_list(COMMA, ident) EQ_ASSIGN es=separated_nonempty_list(COMMA, expr)
    { mk_instr (Vars(ids, None, [mk_instr (Set(List.map (fun id -> mk_expr_loc (Var id) id.loc) ids, es)) $startpos $endpos])) $startpos $endpos }
| VAR ids=separated_nonempty_list(COMMA, ident) t=typ EQ_ASSIGN es=separated_nonempty_list(COMMA, expr)
    { mk_instr (Vars(ids, Some t, [mk_instr (Set(List.map (fun id -> mk_expr_loc (Var id) id.loc) ids, es)) $startpos $endpos])) $startpos $endpos }
;

/* Helper pour le else : accepte soit un bloc, soit un autre if */
compound_stmt_or_block:
| b=block { mk_instr (Block b) $startpos $endpos }
| i=compound_stmt { match i.idesc with If _ -> i | _ -> raise Error }
;

simple_stmt_opt:
| /* empty */ { None }
| s=simple_stmt { Some s }
;

/* === Expressions === */

expr:
| e=expr_desc { mk_expr e $startpos $endpos }
;

expr_desc:
| n=INT         { Int(n) }
| b=BOOL        { Bool(b) }
| s=STRING      { String(s) }
| NIL           { Nil }
| id=ident      { Var(id) }
| e=expr DOT id=ident { Dot(e, id) }
| NEW LPAR s=IDENT RPAR { New(s) }
| id=ident LPAR es=separated_list(COMMA, expr) RPAR { Call(id, es) }
| MINUS e=expr %prec UMINUS { Unop(Opp, e) }
| NOT e=expr                { Unop(Not, e) }
| e1=expr operation=op e2=expr { Binop(operation, e1, e2) }
| LPAR e=expr_desc RPAR { e }
;

op:
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