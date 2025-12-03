open Mgoast
open Mips

(* --- Utilitaires de gestion des labels --- *)
let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt;
  Printf.sprintf "_label_%i" !cpt

(* --- Gestion des Structures (Layout) --- *)
(* On associe à chaque structure la taille totale et l'offset de chaque champ *)
module StringMap = Map.Make(String)

type struct_layout = {
  size: int;
  offsets: int StringMap.t; (* champ -> offset *)
}

(* Table globale des structures *)
let struct_table = ref StringMap.empty

(* Calcule la disposition mémoire d'une structure *)
let compute_struct_layout s =
  let offsets, total_size = 
    List.fold_left (fun (map, offset) (id, _) ->
      (StringMap.add id.id offset map, offset + 4) (* Tout vaut 4 octets (int ou pointeur) *)
    ) (StringMap.empty, 0) s.fields
  in
  { size = total_size; offsets = offsets }

(* --- Environnement de Compilation (Variables) --- *)
type env = {
  vars : int StringMap.t; (* associe un id à un offset par rapport à $fp *)
  next_local : int;       (* prochain offset disponible pour une locale (négatif) *)
  exit_label : string;    (* label de sortie de la fonction courante *)
}

let empty_env = { vars = StringMap.empty; next_local = -4; exit_label = "" }

(* --- Compilation des Expressions --- *)

(* Génère le code pour calculer l'adresse d'une "valeur gauche" (variable ou champ)
   Le résultat (l'adresse) est placé dans $t0 *)
let rec tr_addr env e = match e.edesc with
  | Var id ->
      (try
        let offset = StringMap.find id.id env.vars in
        addi t0 "fp" offset (* $t0 = $fp + offset *)
       with Not_found -> failwith ("Variable inconnue: " ^ id.id))
  
  | Dot (e_struct, id) ->
      tr_expr env e_struct (* $t0 contient l'adresse de la structure (pointeur) *)
      @@ (match e_struct.edesc with (* Petite astuce pour retrouver le nom du type si besoin, sinon on suppose le typage correct *)
          | _ -> 
            (* On suppose que le typage a validé que c'est une struct. 
               Note: Dans un compilateur complet, on annoterait l'AST avec les types.
               Ici, on va devoir tricher ou supposer qu'on connait le type via le contexte, 
               mais le plus simple est de supposer que l'offset est correct. 
               Pour simplifier sans AST typé, on cherche le champ dans TOUTES les structs (incomplet mais fonctionnel pour ce projet simple) *)
            let offset = 
              StringMap.fold (fun _ layout acc -> 
                if StringMap.mem id.id layout.offsets 
                then Some (StringMap.find id.id layout.offsets) 
                else acc
              ) !struct_table None 
            in
            match offset with
            | Some off -> addi t0 t0 off
            | None -> failwith ("Champ inconnu ou ambigu: " ^ id.id))

  | _ -> failwith "Pas une valeur gauche (lvalue)"

(* Génère le code pour évaluer une expression. Le résultat est dans $t0 *)
and tr_expr env e = match e.edesc with
  | Int(n)  -> li t0 (Int64.to_int n)
  | Bool(b) -> li t0 (if b then 1 else 0)
  | Nil     -> li t0 0
  | String(s) -> 
      let l = new_label() in
      la t0 l (* On retournera l'adresse, la chaîne sera dans .data *)
      (* Note: pour faire propre, il faudrait collecter les chaînes à part *)

  | Var _ -> 
      tr_addr env e
      @@ lw t0 0 t0 (* Charge la valeur à l'adresse calculée *)

  | New s_name ->
      let layout = try StringMap.find s_name !struct_table 
                   with Not_found -> failwith ("Structure inconnue: " ^ s_name) in
      li a0 layout.size
      @@ li v0 9      (* Syscall 9: sbrk (malloc) *)
      @@ syscall
      @@ move t0 v0   (* Adresse du bloc alloué dans $t0 *)

  | Dot _ ->
      tr_addr env e
      @@ lw t0 0 t0

  | Call (f, args) ->
      (* 1. Empiler les arguments *)
      let push_args = 
        List.fold_left (fun acc arg -> 
          acc @@ tr_expr env arg @@ push t0
        ) nop args 
      in
      push_args
      @@ jal f.id
      @@ addi sp sp (4 * List.length args) (* Nettoyage de la pile *)
      @@ move t0 v0 (* Le résultat est conventionnellement dans v0, on le remet dans t0 *)

  | Unop(Opp, e) -> tr_expr env e @@ sub t0 "0" t0
  | Unop(Not, e) -> tr_expr env e @@ seq t0 t0 "0" (* xor avec 1 ou seq avec 0 *)

  | Binop(bop, e1, e2) ->
      let op = match bop with
        | Add -> add | Sub -> sub | Mul -> mul | Div -> div | Rem -> rem
        | Lt -> slt | Le -> sle | Gt -> sgt | Ge -> sge | Eq -> seq | Neq -> sne
        | And -> and_ | Or -> or_ 
      in
      tr_expr env e2
      @@ push t0
      @@ tr_expr env e1
      @@ pop t1
      @@ op t0 t0 t1

  | Print(exps) -> 
      let rec print_args = function
        | [] -> nop
        | e::es -> 
            tr_expr env e
            @@ move a0 t0
            @@ li v0 1 (* Print integer *)
            @@ syscall 
            @@ print_args es
      in
      print_args exps @@ li t0 0 (* Print retourne "rien", on met 0 *)

(* --- Compilation des Instructions --- *)

let rec tr_seq env = function
  | []   -> nop
  | [i]  -> tr_instr env i
  | i::s -> tr_instr env i @@ tr_seq env s

and tr_instr env i = match i.idesc with 
  | Expr e -> tr_expr env e

  | Set (lvl, el) ->
      (* Affectation multiple simplifiée : on suppose que lvl et el ont la même longueur.
         On évalue tout à droite (empile), on calcule adresses à gauche, on dépile et stocke. *)
      let push_rhs = 
        List.fold_left (fun code e -> code @@ tr_expr env e @@ push t0) nop el 
      in
      let pop_and_assign = 
        List.fold_right (fun lv code -> 
           code 
           @@ tr_addr env lv   (* $t0 = adresse destination *)
           @@ pop t1           (* $t1 = valeur dépilée *)
           @@ sw t1 0 t0
        ) lvl nop
      in
      push_rhs @@ pop_and_assign

  (* Déclaration de variables : on réserve de la place sur la pile *)
  | Vars (ids, _, seq_body) ->
      let new_env, alloc_code = 
        List.fold_left (fun (e, code) id ->
           let offset = e.next_local in
           let ne = { e with 
                      vars = StringMap.add id.id offset e.vars;
                      next_local = offset - 4 } in
           (ne, code @@ li t0 0 @@ push t0) (* On initialise à 0 par défaut *)
        ) (env, nop) ids
      in
      alloc_code @@ tr_seq new_env seq_body
      (* Note: On ne "libère" pas explicitement ici, sp sera restauré à la fin de la fonction *)

  | If(c, s1, s2) ->
      let then_lbl = new_label() in
      let end_lbl = new_label() in
      tr_expr env c
      @@ bnez t0 then_lbl
      @@ tr_seq env s2
      @@ j end_lbl
      @@ label then_lbl
      @@ tr_seq env s1
      @@ label end_lbl

  | For(c, s) ->
      let test_lbl = new_label() in
      let code_lbl = new_label() in
      j test_lbl
      @@ label code_lbl
      @@ tr_seq env s
      @@ label test_lbl
      @@ tr_expr env c
      @@ bnez t0 code_lbl

  | Block s -> tr_seq env s

  | Return el ->
      (match el with
       | [] -> nop
       | [e] -> tr_expr env e @@ move v0 t0 (* Convention: retour dans v0 *)
       | _ -> failwith "Multi-return non implémenté en MIPS")
      @@ j env.exit_label

  | Inc e -> (* e++ équivaut à e = e + 1 *)
      tr_addr env e @@ push t0 (* Sauve l'adresse *)
      @@ lw t0 0 t0 (* Charge val *)
      @@ addi t0 t0 1
      @@ pop t1 (* Récupère l'adresse *)
      @@ sw t0 0 t1

  | Dec e ->
      tr_addr env e @@ push t0 
      @@ lw t0 0 t0 
      @@ sub t0 t0 "1" 
      @@ pop t1 
      @@ sw t0 0 t1

(* --- Compilation des Fonctions et Programme --- *)

let tr_fun df =
  let exit_lbl = "exit_" ^ df.fname.id in
  
  (* 1. Construire l'environnement initial avec les arguments *)
  (* Convention : Arguments à $fp+8, $fp+12... (car $ra et old $fp sont empilés) *)
  let env_args, _ = 
    List.fold_left (fun (e, off) (id, _) ->
      ({ e with vars = StringMap.add id.id off e.vars }, off + 4)
    ) ({ empty_env with exit_label = exit_lbl }, 8) (List.rev df.params) 
    (* List.rev car le dernier argument est poussé en dernier, donc plus près de $fp si on empile *)
    (* Attention: avec la logique de Call ci-dessus (gauche -> droite + push), 
       le DERNIER argument est en haut de la pile (offset 8), le PREMIER est plus loin. 
       Il faut inverser ou adapter l'offset. *)
  in
  
  (* Correction ordre arguments : si on push arg1 puis arg2 :
     SP -> arg2
           arg1
     Call -> push RA, push FP.
     FP -> old FP
           RA
           arg2 (offset 8)
           arg1 (offset 12)
     Donc Arg N est à 8, Arg 1 est à 8 + 4*(N-1).
  *)
  let env_with_args = 
    let n = List.length df.params in
    List.fold_left (fun (e, idx) (id, _) ->
       (* arg i (0-indexed) est à 8 + 4 * (n - 1 - i) *)
       let off = 8 + 4 * (n - 1 - idx) in
       ({ e with vars = StringMap.add id.id off e.vars }, idx + 1)
    ) ({ empty_env with exit_label = exit_lbl }, 0) df.params |> fst
  in

  (* 2. Corps de la fonction *)
  let code_corps = tr_seq env_with_args df.body in

  (* 3. Assemblage avec prologue/épilogue *)
  label df.fname.id
  @@ push ra          (* Sauvegarde adresse retour *)
  @@ push "fp"        (* Sauvegarde ancien frame pointer *)
  @@ move "fp" sp     (* Nouveau frame pointer *)
  @@ code_corps
  @@ label exit_lbl
  @@ move sp "fp"     (* Restaure la pile (libère locales) *)
  @@ pop "fp"         (* Restaure frame pointer *)
  @@ pop ra           (* Restaure adresse retour *)
  @@ jr ra            (* Retour *)

let tr_main df =
  (* Pour le main, pas d'arguments, on initialise juste *)
  let env = { empty_env with exit_label = "main_exit" } in
  label "main"
  @@ tr_seq env df.body
  @@ label "main_exit"
  @@ li v0 10 
  @@ syscall

let tr_prog decls =
  (* Passe 1 : Collecter les structures *)
  List.iter (function
    | Struct s -> 
        let layout = compute_struct_layout s in
        struct_table := StringMap.add s.sname.id layout !struct_table
    | _ -> ()
  ) decls;

  (* Passe 2 : Compiler les fonctions *)
  let text_seg = 
    List.fold_left (fun code decl -> 
      match decl with
      | Fun f when f.fname.id = "main" -> code @@ tr_main f
      | Fun f -> code @@ tr_fun f
      | _ -> code
    ) nop decls 
  in
  
  (* Segment de données (simplifié) *)
  { text = text_seg; data = nop }