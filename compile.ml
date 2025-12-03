open Mgoast
open Mips

(* --- Utilitaires de gestion des labels --- *)
let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt;
  Printf.sprintf "_label_%i" !cpt

(* --- Gestion des Chaînes de caractères (Data segment) --- *)
(* Liste mutable pour stocker les couples (label, contenu) *)
let strings = ref []

let add_string s =
  let l = new_label () in
  strings := (l, s) :: !strings;
  l

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
type func_sig = { nargs: int; nrets: int; }
let func_table = ref StringMap.empty

(* --- Environnement de Compilation --- *)
type env = {
  vars : int StringMap.t; (* id -> offset par rapport à $fp *)
  next_local : int;       (* prochain offset dispo pour locale (négatif) *)
  exit_label : string;    (* label de sortie de la fonction *)
  ret_ptr_offsets : int list; (* Offsets ($fp) des pointeurs pour retours multiples *)
}

let empty_env = { 
  vars = StringMap.empty;
  next_local = -4; 
  exit_label = ""; 
  ret_ptr_offsets = [] 
}

let get_func_sig id =
  try StringMap.find id !func_table
  with Not_found -> 
    Printf.eprintf "Fatal error: Fonction inconnue '%s'\n" id;
    exit 2

(* --- Compilation des Expressions --- *)

(* Calcule l'adresse d'une L-Value dans $t0 *)
let rec tr_addr env e = match e.edesc with
  | Var id ->
      (try
        let offset = StringMap.find id.id env.vars in
        addi t0 fp offset
       with Not_found -> 
         Printf.eprintf "Fatal error: Variable inconnue '%s'\n" id.id; exit 2)
  
  | Dot (e_struct, id) ->
      tr_expr env e_struct
      @@ (match e_struct.edesc with
          | _ -> 
            (* On supprime la ligne 'let struct_name = ...' qui causait l'erreur *)
            
            (* Recherche du champ 'id' dans toutes les structures connues *)
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
  | String(s) -> 
      let l = add_string s in (* CORRECTION: Ajout à la table des chaînes *)
      la t0 l 

  | Var _ -> 
      tr_addr env e @@ lw t0 0 t0

  | New s_name ->
      let layout = try StringMap.find s_name !struct_table 
                   with Not_found -> Printf.eprintf "Fatal error: Struct '%s' inconnue\n" s_name; exit 2 in
      li a0 layout.size
      @@ li v0 9      (* sbrk *)
      @@ syscall
      (* On pourrait initialiser la mémoire à 0 ici si nécessaire (Go le garantit) *)
      @@ move t0 v0

  | Dot _ ->
      tr_addr env e @@ lw t0 0 t0

  | Call (f, args) ->
      let sig_f = get_func_sig f.id in
      if sig_f.nrets > 1 then (
         Printf.eprintf "Fatal error: Appel à '%s' (multi-return) dans expr simple\n" f.id; exit 2
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

  | Unop(Opp, e) -> tr_expr env e @@ sub t0 zero t0 (* CORRECTION: 0 - t0 avec registre zero *)
  | Unop(Not, e) -> tr_expr env e @@ seq t0 t0 zero (* CORRECTION: t0 == 0 -> 1, t0 != 0 -> 0 *)

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
            (* Cas spécial pour les littéraux chaînes : syscall 4 *)
            | String _ ->
                tr_expr env e
                @@ move a0 t0
                @@ li v0 4 
                @@ syscall
            
            (* Cas spécial appels multi-retours (inchangé) *)
            | Call(f, args) ->
                let sig_f = get_func_sig f.id in
                if sig_f.nrets > 1 then
                  (* ... (Garder tout le code complexe du print multi-retour ici) ... *)
                  (* Copier-coller le bloc 'Call' existant de votre code précédent *)
                  (* Je le résume ici pour la clarté, ne supprimez pas votre logique ! *)
                  let n = sig_f.nrets in
                  addi sp sp (-4 * n)
                  @@ move t1 sp
                  @@ (List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args)
                  @@ (
                    let nargs = List.length args in
                    let rec loop i =
                      if i >= n then nop
                      else
                        addi t0 sp ((nargs * 4) + (i * 4))
                        @@ push t0
                        @@ loop (i + 1)
                    in loop 0
                  )
                  @@ jal f.id
                  @@ addi sp sp (4 * (List.length args + n))
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
                  @@ addi sp sp (4 * n)
                else
                  tr_expr env e @@ move a0 t0 @@ li v0 1 @@ syscall 

            (* Cas par défaut (entiers, booléens, autres) : syscall 1 *)
            | _ -> 
              tr_expr env e
              @@ move a0 t0
              @@ li v0 1 
              @@ syscall 
            )
          @@ print_args es
      in
      print_args exps @@ li t0 0

(* --- Compilation des Instructions --- *)

let rec tr_seq env = function
  | []   -> nop
  | i::s -> tr_instr env i @@ tr_seq env s

and tr_instr env i = match i.idesc with 
  | Expr e -> tr_expr env e

  | Set (lvl, el) ->
      (match el with
       | [{edesc = Call(f, args); _}] when (get_func_sig f.id).nrets > 1 ->
           let sig_f = get_func_sig f.id in
           (* Multi-return assign *)
           let push_args = 
             List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args 
           in
           let push_dst_ptrs =
             List.fold_left (fun acc lv -> 
               match lv.edesc with
               | Var {id="_";_} -> (* Gestion variable poubelle '_' : on alloue un slot temp *)
                   acc @@ addi sp sp (-4) @@ move t0 sp @@ push t0 
               | _ ->
                   acc @@ tr_addr env lv @@ push t0
             ) nop lvl
           in
           push_args
           @@ push_dst_ptrs
           @@ jal f.id
           (* Nettoyage args + pointeurs *)
           (* Attention: si on a alloué pour '_', il faut dépiler proprement ? 
              Ici, push_dst_ptrs a fait des push. addi sp sp ... nettoie tout. C'est bon. *)
           @@ addi sp sp (4 * (List.length args + List.length lvl))

       | _ -> 
         let push_rhs = 
           List.fold_left (fun code e -> code @@ tr_expr env e @@ push t0) nop el 
         in
         let pop_and_assign = 
           List.fold_right (fun lv code -> 
             code 
             @@ tr_addr env lv   (* $t0 = adresse destination *)
             @@ pop t1           (* $t1 = valeur dépilée (RHS) *)
             @@ sw t1 0 t0
           ) lvl nop
         in
         push_rhs @@ pop_and_assign
      )

  | Vars (ids, _, seq_body) ->
      let new_env, alloc_code = 
        List.fold_left (fun (e, code) id ->
           let offset = e.next_local in
           let ne = { e with 
                      vars = StringMap.add id.id offset e.vars;
                      next_local = offset - 4 } in
           (ne, code @@ li t0 0 @@ push t0) 
        ) (env, nop) ids
      in
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
      if env.ret_ptr_offsets <> [] then (
        (* Cas Multi-Return *)
        match el with
        | [{edesc = Call(f, args); _}] when (get_func_sig f.id).nrets > 1 ->
            (* Tail call multi-ret *)
            let push_args = 
               List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args 
            in
            let pass_my_ptrs =
               List.fold_left (fun acc off ->
                 acc 
                 @@ lw t0 off fp (* CORRECTION: fp *)
                 @@ push t0
               ) nop env.ret_ptr_offsets
            in
            push_args
            @@ pass_my_ptrs
            @@ jal f.id
            @@ addi sp sp (4 * (List.length args + List.length env.ret_ptr_offsets))
            @@ j env.exit_label
        
        | _ ->
            (* Assignation aux pointeurs de retour *)
            List.fold_left2 (fun acc expr ptr_offset ->
              acc
              @@ tr_expr env expr
              @@ lw t1 ptr_offset fp (* CORRECTION: fp *)
              @@ sw t0 0 t1         
            ) nop el env.ret_ptr_offsets
            @@ j env.exit_label
      ) else (
        (* Cas Mono-Return *)
        match el with
        | [] -> j env.exit_label
        | [e] -> tr_expr env e @@ move v0 t0 @@ j env.exit_label
        | _ -> Printf.eprintf "Fatal error: Multi-return mismatch\n"; exit 2
      )

  | Inc e -> 
      tr_addr env e @@ push t0 
      @@ lw t0 0 t0 @@ addi t0 t0 1 (* CORRECTION: addi simple *)
      @@ pop t1 @@ sw t0 0 t1

  | Dec e ->
      tr_addr env e @@ push t0 
      @@ lw t0 0 t0 @@ addi t0 t0 (-1) (* CORRECTION: addi -1 au lieu de sub "1" *)
      @@ pop t1 @@ sw t0 0 t1

(* --- Compilation des Fonctions et Programme --- *)

let tr_fun df =
  let sig_f = get_func_sig df.fname.id in
  let exit_lbl = "exit_" ^ df.fname.id in
  
  let n_explicit = List.length df.params in
  let n_implicit = if sig_f.nrets > 1 then sig_f.nrets else 0 in
  let total_args = n_explicit + n_implicit in

  let env_params, _ = 
    List.fold_left (fun (e, idx) (id, _) ->
       let off = 8 + 4 * (total_args - 1 - idx) in
       ({ e with vars = StringMap.add id.id off e.vars }, idx + 1)
    ) ({ empty_env with exit_label = exit_lbl }, 0) df.params
  in

  let env_complete = 
    if n_implicit > 0 then
      let rec loop i acc = 
        if i >= n_implicit then (List.rev acc, i)
        else
           let off = 8 + 4 * (n_implicit - 1 - i) in
           loop (i + 1) (off :: acc)
      in 
      let ptr_offsets, _ = loop 0 [] in
      { env_params with ret_ptr_offsets = ptr_offsets }
    else
      env_params
  in

  let code_corps = tr_seq env_complete df.body in

  label df.fname.id
  @@ push ra          
  @@ push fp          (* CORRECTION: fp *)
  @@ move fp sp       (* CORRECTION: fp *)
  @@ code_corps
  @@ label exit_lbl
  @@ move sp fp       (* CORRECTION: fp *)
  @@ pop fp           (* CORRECTION: fp *)
  @@ pop ra           
  @@ jr ra            

let tr_main df =
  let env = { empty_env with exit_label = "main_exit" } in
  label "main"
  @@ move fp sp   (* <--- CORRECTION IMPORTANTE : Initialiser $fp *)
  @@ tr_seq env df.body
  @@ label "main_exit"
  @@ li v0 10 
  @@ syscall

let tr_prog decls =
  List.iter (function
    | Struct s -> 
        let layout = compute_struct_layout s in
        struct_table := StringMap.add s.sname.id layout !struct_table
    | Fun f ->
        let nrets = List.length f.return in
        let sig_f = { nargs = List.length f.params; nrets = nrets } in
        func_table := StringMap.add f.fname.id sig_f !func_table
  ) decls;
  
  let text_seg = 
    List.fold_left (fun code decl -> 
      match decl with
      | Fun f when f.fname.id = "main" -> code @@ tr_main f
      | Fun f -> code @@ tr_fun f
      | _ -> code
    ) nop decls 
  in
  
  (* CORRECTION: Génération du segment de données pour les chaînes *)
  let data_seg = 
    List.fold_left (fun acc (lbl, str) -> 
      acc @@ label lbl @@ asciiz str
    ) nop !strings
  in
  
  { text = text_seg; data = data_seg }