open Mgoast
open Mips

let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "_label_%i" !cpt

(* le résultat de l'expression est dans le registre $t0,
   la pile est utilisée pour les valeurs intermédiaires *)
let rec tr_expr e = match e.edesc with
  | Int(n)  -> li t0 (Int64.to_int n)   (* on supposera que les constantes entières
                                           sont représentables sur 32 bits *)
  | Bool(b) -> li t0 (if b then 1 else 0) (* Booléens: 0 ou 1 *)
  | String(s) -> failwith "Chaînes à faire plus tard (allocation statique)"
  | Var(id) -> failwith "Variables à faire plus tard"
  | Binop(bop, e1, e2) ->
    let op = match bop with
      | Add -> add
      | Sub -> sub
      | Mul -> mul
      | Div -> div
      | Rem -> rem
      | Lt  -> slt
      | Le  -> sle
      | Gt  -> sgt
      | Ge  -> sge
      | Eq  -> seq
      | Neq -> sne
      | And -> and_
      | Or  -> or_
      (* Note: Pour And/Or, MIPS n'a pas de && court-circuité par défaut, 
         mais 'and' bit-à-bit fonctionne si on utilise 0 et 1 pour bool *)
    in
    tr_expr e2        (* 1. Calculer partie droite *)
    @@ push t0        (* 2. Sauvegarder sur la pile *)
    @@ tr_expr e1     (* 3. Calculer partie gauche (résultat dans t0) *)
    @@ pop t1         (* 4. Récupérer partie droite dans t1 *)
    @@ op t0 t0 t1    (* 5. t0 = t0 op t1 *)

  | Unop(Opp, e) ->
      tr_expr e
      @@ Mips.S "  neg $t0, $t0" (* Négation arithmétique *)
      
  | Unop(Not, e) ->
      tr_expr e
      @@ xor t0 t0 t0 (* Si t0=1 -> 0, si t0=0 -> 1 ? Non. *)
      (* Astuce MIPS pour NOT logique sur 0/1 : seq $t0, $t0, 0 *)
      @@ seq t0 t0 "0"
  | Print(exps) -> 
      let rec print_args = function
        | [] -> nop
        | e::es -> 
            tr_expr e          (* Calcule e -> résultat dans $t0 *)
            @@ move a0 t0      (* Déplace $t0 dans $a0 pour syscall *)
            @@ li v0 1         (* Code 1: print_int *)
            @@ syscall 
            @@ print_args es
      in
      print_args exps 
      @@ li t0 0
  | _ -> failwith "A compléter"


let rec tr_seq = function
  | []   -> nop
  | [i]  -> tr_instr i
  | i::s -> tr_instr i @@ tr_seq s

and tr_instr i = match i.idesc with 
  | If(c, s1, s2) ->
    let then_label = new_label()
    and end_label = new_label()
    in
    tr_expr c
    @@ bnez t0 then_label
    @@ tr_seq s2
    @@ j end_label
    @@ label then_label
    @@ tr_seq s1
    @@ label end_label

  | For(c, s) ->
    let test_label = new_label()
    and code_label = new_label()
    in
    j test_label
    @@ label code_label
    @@ tr_seq s
    @@ label test_label
    @@ tr_expr c
    @@ bnez t0 code_label
  | Expr e -> 
    tr_expr e  (* On génère le code de l'expression *)
      
  | Block s -> 
    tr_seq s
  | _ -> failwith "A compléter"

let tr_fun df =
  let code_corps = tr_seq df.body in
  let code_final = 
    if df.fname.id = "main" then
      code_corps @@ li v0 10 @@ syscall (* Ajout de l'exit pour le main *)
    else
      code_corps @@ jr ra (* Retour classique pour les autres fonctions (à gérer plus tard si besoin) *)
  in
  label df.fname.id @@ code_final

let rec tr_ldecl = function
    Fun df::p -> tr_fun df @@ tr_ldecl p
  | _ :: p -> tr_ldecl p
  | [] -> nop

let tr_prog p =  { text = tr_ldecl p ; data = nop }
