open Mgoast
open Mips

(* --- Utilitaires de gestion des labels --- *)
let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt;
  Printf.sprintf "_label_%i" !cpt

(* --- Gestion des Structures (Layout) --- *)
module StringMap = Map.Make(String)

type struct_layout = {
  size: int;
  offsets: int StringMap.t; (* champ -> offset *)
}

let struct_table = ref StringMap.empty

let compute_struct_layout s =
  let offsets, total_size = 
    List.fold_left (fun (map, offset) (id, _) ->
      (StringMap.add id.id offset map, offset + 4)
    ) (StringMap.empty, 0) s.fields
  in
  { size = total_size; offsets = offsets }

(* --- Gestion des Fonctions (Signatures) --- *)
type func_sig = {
  nargs: int;
  nrets: int;
}
(* Table globale des fonctions pour connaître leur arité au moment de l'appel *)
let func_table = ref StringMap.empty

(* --- Environnement de Compilation --- *)
type env = {
  vars : int StringMap.t;     (* id -> offset par rapport à $fp *)
  next_local : int;           (* prochain offset dispo pour locale (négatif) *)
  exit_label : string;        (* label de sortie de la fonction *)
  ret_ptr_offsets : int list; (* Offsets ($fp) des pointeurs pour retours multiples (vide si nrets <= 1) *)
}

let empty_env = { 
  vars = StringMap.empty; 
  next_local = -4; 
  exit_label = ""; 
  ret_ptr_offsets = [] 
}

(* Récupère la signature d'une fonction *)
let get_func_sig id =
  try StringMap.find id !func_table
  with Not_found -> 
    (* Cas des fonctions primitives ou intrinsèques si besoin, sinon erreur *)
    Printf.eprintf "Fatal error: Fonction inconnue '%s' lors de la compilation\n" id;
    exit 2

(* --- Compilation des Expressions --- *)

(* Calcule l'adresse d'une L-Value dans $t0 *)
let rec tr_addr env e = match e.edesc with
  | Var id ->
      (try
        let offset = StringMap.find id.id env.vars in
        addi t0 "fp" offset
       with Not_found -> 
         Printf.eprintf "Fatal error: Variable inconnue '%s'\n" id.id; exit 2)
  
  | Dot (e_struct, id) ->
      tr_expr env e_struct
      @@ (match e_struct.edesc with
          | _ -> 
            let offset = 
              StringMap.fold (fun _ layout acc -> 
                if StringMap.mem id.id layout.offsets 
                then Some (StringMap.find id.id layout.offsets) 
                else acc
              ) !struct_table None 
            in
            match offset with
            | Some off -> addi t0 t0 off
            | None -> Printf.eprintf "Fatal error: Champ inconnu '%s'\n" id.id; exit 2)

  | _ -> Printf.eprintf "Fatal error: L-value attendue\n"; exit 2

(* Évalue une expression et met le résultat dans $t0 *)
and tr_expr env e = match e.edesc with
  | Int(n)  -> li t0 (Int64.to_int n)
  | Bool(b) -> li t0 (if b then 1 else 0)
  | Nil     -> li t0 0
  | String(_) -> 
      let l = new_label() in
      la t0 l 

  | Var _ -> 
      tr_addr env e @@ lw t0 0 t0

  | New s_name ->
      let layout = try StringMap.find s_name !struct_table 
                   with Not_found -> Printf.eprintf "Fatal error: Struct '%s' inconnue\n" s_name; exit 2 in
      li a0 layout.size
      @@ li v0 9      (* sbrk *)
      @@ syscall
      @@ move t0 v0

  | Dot _ ->
      tr_addr env e @@ lw t0 0 t0

  | Call (f, args) ->
      (* Cas standard (1 retour) ou expression (0 retour). 
         Si nrets > 1, c'est géré dans Set/Vars ou Print, pas ici (sauf erreur typage). *)
      let sig_f = get_func_sig f.id in
      if sig_f.nrets > 1 then (
         Printf.eprintf "Fatal error: Appel à '%s' (multi-return) dans un contexte d'expression simple non supporté (sauf Print)\n" f.id;
         exit 2
      );
      
      let push_args = 
        List.fold_left (fun acc arg -> 
          acc @@ tr_expr env arg @@ push t0
        ) nop args 
      in
      push_args
      @@ jal f.id
      @@ addi sp sp (4 * List.length args)
      @@ move t0 v0

  | Unop(Opp, e) -> tr_expr env e @@ sub t0 "0" t0
  | Unop(Not, e) -> tr_expr env e @@ seq t0 t0 "0"

  | Binop(bop, e1, e2) ->
      let op = match bop with
        | Add -> add | Sub -> sub | Mul -> mul | Div -> div | Rem -> rem
        | Lt -> slt | Le -> sle | Gt -> sgt | Ge -> sge | Eq -> seq | Neq -> sne
        | And -> and_ | Or -> or_ 
      in
      tr_expr env e2 @@ push t0
      @@ tr_expr env e1 @@ pop t1
      @@ op t0 t0 t1

  | Print(exps) -> 
      let rec print_args = function
        | [] -> nop
        | e::es -> 
            (match e.edesc with
            | Call(f, args) ->
                let sig_f = get_func_sig f.id in
                if sig_f.nrets > 1 then
                  (* Cas Spécial : Print(f()) où f retourne plusieurs valeurs *)
                  (* 1. Allouer de la place pour les résultats (temps) *)
                  let size_rets = sig_f.nrets * 4 in
                  addi sp sp (-size_rets) (* Réserve l'espace *)
                  
                  (* 2. Empiler les arguments de la fonction *)
                  @@ (List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args)
                  
                  (* 3. Empiler les adresses des résultats *)
                  (* Les résultats sont à $sp + (nargs*4) + 0, +4... 
                     MAIS $sp bouge avec les push des args.
                     Calculons les adresses RELATIVES avant le call. *)
                  (* On push les pointeurs. L'ordre importe peu tant que tr_fun et ici sont d'accord.
                     Convention : on push &res1, &res2... *)
                  @@ (
                    let rec push_ptrs i =
                      if i >= sig_f.nrets then nop
                      else
                        (* Adresse du slot i : il est actuellement à $sp + (args_size) + (i*4) *)
                        (* Mais on a déjà pushé 'args'. Donc sp est plus bas. *)
                        (* Simplification : Utilisons $fp ou calculons. *)
                        (* Mieux : calculons l'adresse absolue des slots temporaires et pushons la. *)
                        (* Les slots ont été alloués tout au début : $sp_init - 4, etc. *)
                        (* C'est compliqué de suivre SP. 
                           Astuce : On copie SP dans un registre temp (s0?) ou on calcule. *)
                        
                        (* Approche "propre" : *)
                        (* Stack state: [ ... Old Stack ... | RetSlots (size_rets) | Args (size_args) | Ptrs (current) ] *)
                        (* L'adresse de RetSlot[i] est : $sp + (size_ptrs_pushed) + size_args + (size_rets - 4 - i*4) ? Non. *)
                        
                        (* Faisons simple : 
                           1. Allouer RetSlots.
                           2. Calculer leurs adresses et les stocker dans des registres ou les pusher direct ?
                           Non, on doit pusher les args d'abord.
                        *)
                        nop 
                    in nop
                  )
                  (* RE-APPROCHE SIMPLE pour Print(multi-return) : 
                     On a besoin de N slots.
                     1. On calcule l'adresse de ces N slots (sur la pile actuelle).
                     2. On push les args.
                     3. On push les adresses des slots.
                     4. Call.
                     5. On clean args + ptrs.
                     6. On print les valeurs des slots.
                     7. On clean slots.
                  *)
                   @@ (
                     let n = sig_f.nrets in
                     (* Allouer N slots *)
                     addi sp sp (-4 * n)
                     @@ move t1 sp (* t1 pointe sur le début des résultats *)
                     
                     (* Empiler les arguments *)
                     @@ (List.fold_left (fun acc arg -> 
                           acc @@ tr_expr env arg @@ push t0
                        ) nop args)
                     
                     (* Empiler les pointeurs vers les slots (qui sont à t1) *)
                     (* Attention t1 est valide tant qu'on n'y touche pas. tr_expr touche t1.
                        On doit être prudent. *)
                     (* Solution : On calcule l'adresse de chaque slot RELATIVEMENT à SP actuel. *)
                     (* SP actuel = SP_apres_slots - (nargs*4).
                        Les slots sont à : SP_actuel + (nargs*4) + (0..N-1)*4 *)
                     
                     @@ (
                       let nargs = List.length args in
                       let rec loop i =
                         if i >= n then nop
                         else
                           (* Adresse du slot i (0 à n-1) *)
                           (* Slot 0 est le premier retour (celui en haut des slots, adresse basse) *)
                           (* Offset = (nargs * 4) + (n - 1 - i) * 4 si on veut suivre l'ordre d'empilement *)
                           (* On va dire : slot 0 est à l'adresse la plus basse (SP_apres_slots) *)
                           (* Offset = (nargs * 4) + (i * 4) *)
                           addi t0 sp ((nargs * 4) + (i * 4))
                           @@ push t0
                           @@ loop (i + 1)
                       in loop 0
                     )
                     
                     @@ jal f.id
                     @@ addi sp sp (4 * (List.length args + n)) (* Clean args + ptrs *)
                     
                     (* Maintenant on affiche les valeurs stockées dans les slots *)
                     (* Les slots sont au sommet de la pile (sp pointe dessus) *)
                     @@ (
                       let rec print_slots i =
                         if i >= n then nop
                         else
                           lw a0 (i*4) sp
                           @@ li v0 1
                           @@ syscall
                           @@ print_slots (i + 1)
                       in print_slots 0
                     )
                     @@ addi sp sp (4 * n) (* Clean slots *)
                   )

                else (* Cas simple : Call renvoie 1 valeur *)
                  tr_expr env e @@ move a0 t0 @@ li v0 1 @@ syscall 

            | _ -> (* Cas simple : Expression standard *)
              tr_expr env e
              @@ move a0 t0
              @@ li v0 1 (* Print integer *)
              @@ syscall 
            )
            @@ print_args es
      in
      print_args exps @@ li t0 0

(* --- Compilation des Instructions --- *)

let rec tr_seq env = function
  | []   -> nop
  | [i]  -> tr_instr env i
  | i::s -> tr_instr env i @@ tr_seq env s

and tr_instr env i = match i.idesc with 
  | Expr e -> tr_expr env e

  | Set (lvl, el) ->
      (* Détection du cas spécial : Appel multi-return unique à droite *)
      (match el with
       | [{edesc = Call(f, args); _}] when (get_func_sig f.id).nrets > 1 ->
           let sig_f = get_func_sig f.id in
           (* Vérification arité (déjà faite par typechecker normalement) *)
           if List.length lvl <> sig_f.nrets then (
             Printf.eprintf "Fatal error: Mismatch assignation multi-return\n"; exit 2
           );
           
           (* 1. Empiler les arguments de la fonction *)
           let push_args = 
             List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args 
           in
           
           (* 2. Empiler les adresses des variables de destination *)
           let push_dst_ptrs =
             List.fold_left (fun acc lv -> 
               acc @@ tr_addr env lv @@ push t0
             ) nop lvl
           in
           
           push_args
           @@ push_dst_ptrs
           @@ jal f.id
           (* Nettoyage : args + pointeurs *)
           @@ addi sp sp (4 * (List.length args + List.length lvl))

       | _ -> 
         (* Cas standard : autant d'expressions que de variables *)
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
      )

  (* Déclaration de variables avec initialisation potentiellement multi-return *)
  | Vars (ids, _, seq_body) ->
      (* Dans l'AST, Vars contient soit une init vide, soit une séquence d'init (Set) *)
      (* On alloue d'abord l'espace *)
      let new_env, alloc_code = 
        List.fold_left (fun (e, code) id ->
           let offset = e.next_local in
           let ne = { e with 
                      vars = StringMap.add id.id offset e.vars;
                      next_local = offset - 4 } in
           (ne, code @@ li t0 0 @@ push t0) 
        ) (env, nop) ids
      in
      
      (* Le corps (seq_body) contient l'instruction Set d'initialisation si présente *)
      (* tr_seq utilisera le Set modifié ci-dessus pour gérer le multi-return *)
      alloc_code @@ tr_seq new_env seq_body

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
      (* Gestion du retour : simple ou multiple *)
      if env.ret_ptr_offsets <> [] then (
        (* Cas Multi-Return *)
        (* On doit avoir autant d'expressions que de pointeurs de retour *)
        
        (* Cas spécial : Return f() où f retourne plusieurs valeurs (Tail Call style) *)
        match el with
        | [{edesc = Call(f, args); _}] when (get_func_sig f.id).nrets > 1 ->
            (* On appelle f en lui passant NOS pointeurs de retour *)
            let push_args = 
               List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args 
            in
            let pass_my_ptrs =
               List.fold_left (fun acc off ->
                 acc 
                 @@ lw t0 off "fp" (* Charge le pointeur reçu *)
                 @@ push t0        (* Le passe à f *)
               ) nop env.ret_ptr_offsets
            in
            push_args
            @@ pass_my_ptrs
            @@ jal f.id
            @@ addi sp sp (4 * (List.length args + List.length env.ret_ptr_offsets))
            @@ j env.exit_label
        
        | _ ->
            (* Cas standard : return e1, e2... *)
            if List.length el <> List.length env.ret_ptr_offsets then (
               Printf.eprintf "Fatal error: Return arity mismatch\n"; exit 2
            );
            
            List.fold_left2 (fun acc expr ptr_offset ->
              acc
              @@ tr_expr env expr    (* Calcule la valeur -> $t0 *)
              @@ lw t1 ptr_offset "fp" (* Charge l'adresse de destination (pointeur) -> $t1 *)
              @@ sw t0 0 t1          (* Écrit la valeur à l'adresse *)
            ) nop el env.ret_ptr_offsets
            @@ j env.exit_label
      ) else (
        (* Cas Mono-Return (standard) *)
        match el with
        | [] -> j env.exit_label
        | [e] -> tr_expr env e @@ move v0 t0 @@ j env.exit_label
        | _ -> Printf.eprintf "Fatal error: Multi-return dans une fonction mono-return ?\n"; exit 2
      )

  | Inc e -> 
      tr_addr env e @@ push t0 
      @@ lw t0 0 t0 @@ addi t0 t0 1
      @@ pop t1 @@ sw t0 0 t1

  | Dec e ->
      tr_addr env e @@ push t0 
      @@ lw t0 0 t0 @@ sub t0 t0 "1" 
      @@ pop t1 @@ sw t0 0 t1

(* --- Compilation des Fonctions et Programme --- *)

let tr_fun df =
  let sig_f = get_func_sig df.fname.id in
  let exit_lbl = "exit_" ^ df.fname.id in
  
  (* 1. Calcul des offsets des arguments "classiques" *)
  (* Stack frame : 
     ...
     Arg N (ptr retour M si multi)
     ...
     Arg 1
     Arg 0
     RA
     Old FP <- FP
  *)
  
  (* Liste complète des arguments attendus sur la pile : Params explicites + Ptrs implicites *)
  let n_explicit = List.length df.params in
  let n_implicit = if sig_f.nrets > 1 then sig_f.nrets else 0 in
  let total_args = n_explicit + n_implicit in

  (* Environnement initial *)
  (* Les arguments sont à $fp + 8 + (total_args - 1 - index)*4 *)
  
  (* A. Ajouter les paramètres explicites à l'env *)
  let env_params, _ = 
    List.fold_left (fun (e, idx) (id, _) ->
       let off = 8 + 4 * (total_args - 1 - idx) in
       ({ e with vars = StringMap.add id.id off e.vars }, idx + 1)
    ) ({ empty_env with exit_label = exit_lbl }, 0) df.params
  in

  (* B. Identifier les offsets des pointeurs de retour implicites *)
  let env_complete = 
    if n_implicit > 0 then
      let ptr_offsets, _ = 
        (* Les pointeurs sont après les params explicites. Indices : n_explicit à total_args-1 *)
        (* Ptr 0 correspond au premier retour. *)
        (* Si on a pushé args puis ptrs : 
           Stack: [Arg0 ... ArgK Ptr0 ... PtrM]
           PtrM est au sommet (adresse basse relative aux args), Ptr0 ensuite...
           Wait. L'appelant fait : push args; push ptrs.
           Ordre push : Arg0, Arg1, ..., Ptr0, Ptr1.
           Stack (basse adresse) : Ptr1, Ptr0, Arg1, Arg0.
           Donc Ptr1 est le dernier pushé -> $fp+8.
           Ptr0 -> $fp+12.
           
           Donc Ptr_i est à : $fp + 8 + 4 * (n_implicit - 1 - i).
        *)
        let rec loop i acc = 
          if i >= n_implicit then (List.rev acc, i) (* On veut liste [off_ptr0; off_ptr1...] *)
          else
             let off = 8 + 4 * (n_implicit - 1 - i) in
             loop (i + 1) (off :: acc)
        in loop 0 []
      in
      { env_params with ret_ptr_offsets = ptr_offsets }
    else
      env_params
  in

  (* 2. Corps de la fonction *)
  let code_corps = tr_seq env_complete df.body in

  (* 3. Assemblage *)
  label df.fname.id
  @@ push ra          
  @@ push "fp"        
  @@ move "fp" sp     
  @@ code_corps
  @@ label exit_lbl
  @@ move sp "fp"     
  @@ pop "fp"         
  @@ pop ra           
  @@ jr ra            

let tr_main df =
  let env = { empty_env with exit_label = "main_exit" } in
  label "main"
  @@ tr_seq env df.body
  @@ label "main_exit"
  @@ li v0 10 
  @@ syscall

let tr_prog decls =
  (* Passe 1 : Collecter structures ET signatures de fonctions *)
  List.iter (function
    | Struct s -> 
        let layout = compute_struct_layout s in
        struct_table := StringMap.add s.sname.id layout !struct_table
    | Fun f ->
        let nrets = List.length f.return in
        let sig_f = { nargs = List.length f.params; nrets = nrets } in
        func_table := StringMap.add f.fname.id sig_f !func_table
  ) decls;

  (* Passe 2 : Compiler *)
  let text_seg = 
    List.fold_left (fun code decl -> 
      match decl with
      | Fun f when f.fname.id = "main" -> code @@ tr_main f
      | Fun f -> code @@ tr_fun f
      | _ -> code
    ) nop decls 
  in
  
  { text = text_seg; data = nop }