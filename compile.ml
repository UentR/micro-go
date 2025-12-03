(* compile.ml *)
open Mgoast
open Mips

(* --- Utilitaires de gestion des labels --- *)
let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt;
  Printf.sprintf "_label_%i" !cpt

(* --- Gestion des Chaînes de caractères (Data segment) --- *)
let strings = ref []
let add_string s =
  let l = new_label () in
  strings := (l, s) :: !strings;
  l

(* --- Gestion des Structures (Layout) --- *)
module StringMap = Map.Make(String)

type struct_layout = {
  size: int;
  offsets: (int * typ) StringMap.t; 
}

let struct_table = ref StringMap.empty

let compute_struct_layout s =
  let offsets, total_size = 
    List.fold_left (fun (map, offset) (id, t) ->
      (StringMap.add id.id (offset, t) map, offset + 4)
    ) (StringMap.empty, 0) s.fields
  in
  { size = total_size;
  offsets = offsets }

(* --- Gestion des Fonctions (Signatures) --- *)
type func_sig = { 
  nargs: int;
  rets: typ list; 
}
let func_table = ref StringMap.empty

let get_func_sig id =
  try StringMap.find id !func_table
  with Not_found -> 
    Printf.eprintf "Fatal error: Fonction inconnue '%s'\n" id;
  exit 2

(* --- Environnement de Compilation --- *)
type env = {
  vars : (int * typ) StringMap.t;
  next_local : int;
  exit_label : string;
  ret_ptr_offsets : int list;
}

let empty_env = { 
  vars = StringMap.empty;
  next_local = -4;
  exit_label = "";
  ret_ptr_offsets = [] 
}

(* --- Mini-Inférence de type --- *)
let rec get_expr_type env e = match e.edesc with
  | Int _ -> [TInt]
  | Bool _ -> [TBool]
  | String _ -> [TString]
  | Nil -> [TInt] 
  | New s -> [TStruct s]
  | Var id -> 
      (try let (_, t) = StringMap.find id.id env.vars in [t]
       with Not_found -> Printf.eprintf "Var inconnue %s\n" id.id; exit 2)
  | Dot (e_s, id) ->
      (match get_expr_type env e_s with
       | [TStruct s_name] ->
           let layout = StringMap.find s_name !struct_table in
           let (_, t) = StringMap.find id.id layout.offsets in
           [t]
       | _ -> exit 2) 
  | Call (f, _) -> (get_func_sig f.id).rets
  | Unop (Opp, _) -> [TInt]
  | Unop (Not, _) -> [TBool]
  | Binop (op, _, _) ->
      (match op with
       | Add | Sub | Mul | Div | Rem -> [TInt]
       | _ -> [TBool]) 
  | Print _ -> []

(* --- Génération des fonctions d'impression de structures (AVANCÉ) --- *)
let compile_struct_printers () =
  StringMap.fold (fun sname layout code ->
    let print_label = "print_struct_" ^ sname in
    
    (* On trie les champs par offset pour les afficher dans l'ordre *)
    let fields_sorted = 
      List.sort (fun (_, (o1, _)) (_, (o2, _)) -> compare o1 o2) 
                (StringMap.bindings layout.offsets) 
    in
    
    let nil_label = new_label () in
    
    (* Chaînes constantes pour l'affichage *)
    let str_start = add_string "&{" in
    let str_end = add_string "}" in
    let str_nil = add_string "<nil>" in
    let str_space = add_string " " in
    
    let print_code = 
        label print_label
        (* Vérifier si le pointeur est nil *)
        @@ beqz a0 nil_label
        
        (* Sauvegarder $s0 car on va l'utiliser comme pointeur de base stable *)
        @@ addi sp sp (-4) @@ sw s0 0(sp)
        @@ move s0 a0 (* s0 = adresse de la structure *)
        
        (* Afficher "&{" *)
        @@ la a0 str_start @@ li v0 4 @@ syscall
        
        (* Boucle sur les champs *)
        @@ (
          let rec loop = function
            | [] -> nop
            | (_, (off, t)) :: rest ->
                 (* Charger la valeur du champ *)
                 lw a0 off s0
                 @@ (match t with
                     | TString -> li v0 4 @@ syscall
                     | TStruct _ -> 
                        (* Pour les structures imbriquées, on affiche l'adresse (entier)
                           pour éviter les récursions infinies potentielles, 
                           ou on pourrait appeler récursivement le printer.
                           Ici, on reste simple : adresse *)
                        li v0 1 @@ syscall 
                     | _ -> li v0 1 @@ syscall (* Int, Bool -> Int *)
                    )
                 (* Espace si ce n'est pas le dernier champ *)
                 @@ (if rest <> [] then la a0 str_space @@ li v0 4 @@ syscall else nop)
                 @@ loop rest
          in loop fields_sorted
        )
        
        (* Afficher "}" *)
        @@ la a0 str_end @@ li v0 4 @@ syscall
        
        (* Restaurer $s0 et retour *)
        @@ lw s0 0(sp) @@ addi sp sp 4
        @@ jr ra
        
        (* Cas nil *)
        @@ label nil_label
        @@ la a0 str_nil @@ li v0 4 @@ syscall
        @@ jr ra
    in
    code @@ print_code
  ) !struct_table nop

(* --- Compilation des Expressions --- *)

let rec tr_addr env e = match e.edesc with
  | Var id ->
      (try
        let (offset, _) = StringMap.find id.id env.vars in
        addi t0 fp offset
       with Not_found -> exit 2)
  | Dot (e_struct, id) ->
      tr_expr env e_struct
      @@ (match get_expr_type env e_struct with
          | [TStruct s_name] -> 
             let layout = StringMap.find s_name !struct_table in
             let (off, _) = StringMap.find id.id layout.offsets in
             addi t0 t0 off
          | _ -> exit 2)
  | _ -> Printf.eprintf "Fatal error: L-value attendue\n"; exit 2

and tr_expr env e = match e.edesc with
  | Int(n)  -> li t0 (Int64.to_int n)
  | Bool(b) -> li t0 (if b then 1 else 0)
  | Nil     -> li t0 0
  | String(s) -> let l = add_string s in la t0 l 
  | Var _ -> tr_addr env e @@ lw t0 0 t0
  | New s_name ->
      let layout = try StringMap.find s_name !struct_table with Not_found -> exit 2 in
      let loop_lbl = new_label() in
      let end_lbl = new_label() in
      li a0 layout.size @@ li v0 9 @@ syscall @@ move t0 v0
      @@ li t1 0 @@ label loop_lbl @@ li t2 layout.size @@ bge t1 t2 end_lbl
      @@ add t2 t0 t1 @@ sw zero 0 t2 @@ addi t1 t1 4 @@ j loop_lbl @@ label end_lbl
  | Dot _ -> tr_addr env e @@ lw t0 0 t0
  | Call (f, args) ->
      let sig_f = get_func_sig f.id in
      if List.length sig_f.rets > 1 then (Printf.eprintf "Call error\n"; exit 2);
      let push_args = List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args in
      push_args @@ jal f.id @@ addi sp sp (4 * List.length args) @@ move t0 v0
  | Unop(Opp, e) -> tr_expr env e @@ sub t0 zero t0
  | Unop(Not, e) -> tr_expr env e @@ seq t0 t0 zero
  | Binop(bop, e1, e2) ->
      let op = match bop with
        | Add -> add | Sub -> sub | Mul -> mul | Div -> div | Rem -> rem
        | Lt -> slt | Le -> sle | Gt -> sgt | Ge -> sge | Eq -> seq | Neq -> sne
        | And -> and_ | Or -> or_ 
      in
      tr_expr env e2 @@ push t0 @@ tr_expr env e1 @@ pop t1 @@ op t0 t0 t1
  | Print(exps) -> 
      let print_space = li a0 32 @@ li v0 11 @@ syscall in
      let compile_print_single e = 
        match e.edesc with
        | Call(f, args) when List.length (get_func_sig f.id).rets > 1 ->
            let sig_f = get_func_sig f.id in
            let rets = sig_f.rets in
            let n_rets = List.length rets in
            let n_args = List.length args in
            let alloc_res = let rec loop i = if i = 0 then nop else (push zero) @@ loop (i-1) in loop n_rets in
            let push_args_code = List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args in
            let push_ptrs = 
              let rec loop i = if i >= n_rets then nop else 
                 let off = 4 * n_args + 4 * (n_rets - 1 - i) + (i * 4) in
                 addi t0 sp off @@ push t0 @@ loop (i + 1) in loop 0 in
            let call_code = jal f.id in
            let cleanup = addi sp sp (4 * (n_args + n_rets)) in
            let print_code = 
              let rec loop i = if i >= n_rets then nop else
                 let off = 4 * (n_rets - 1 - i) in
                 let t = List.nth rets i in
                 lw t0 off sp @@ 
                 (match t with 
                  | TString -> move a0 t0 @@ li v0 4 @@ syscall 
                  | TStruct s -> move a0 t0 @@ jal ("print_struct_" ^ s) (* Appel printer struct *)
                  | _ -> move a0 t0 @@ li v0 1 @@ syscall)
                 @@ (if i < n_rets - 1 then print_space else nop) @@ loop (i+1) in loop 0 in
            alloc_res @@ push_args_code @@ push_ptrs @@ call_code @@ cleanup @@ print_code @@ addi sp sp (4 * n_rets)
        | _ ->
            let type_list = get_expr_type env e in
            match type_list with
            | [TString] -> tr_expr env e @@ move a0 t0 @@ li v0 4 @@ syscall
            | [TStruct s] -> tr_expr env e @@ move a0 t0 @@ jal ("print_struct_" ^ s) (* Appel printer struct *)
            | _ -> tr_expr env e @@ move a0 t0 @@ li v0 1 @@ syscall
      in
      let rec print_args = function
        | [] -> nop
        | [e] -> compile_print_single e
        | e::es -> compile_print_single e @@ print_space @@ print_args es
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
       | [{edesc = Call(f, args); _}] when List.length (get_func_sig f.id).rets > 1 ->
           let push_args = List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args in
           let push_dst = List.fold_left (fun acc lv -> 
               match lv.edesc with Var {id="_";_} -> acc @@ addi sp sp (-4) @@ move t0 sp @@ push t0 
               | _ -> acc @@ tr_addr env lv @@ push t0) nop lvl in
           push_args @@ push_dst @@ jal f.id @@ addi sp sp (4 * (List.length args + List.length lvl))
       | _ -> 
         let push_rhs = List.fold_left (fun code e -> code @@ tr_expr env e @@ push t0) nop el in
         let pop_assign = List.fold_right (fun lv code -> code @@ tr_addr env lv @@ pop t1 @@ sw t1 0 t0) lvl nop in
         push_rhs @@ pop_assign)
  | Vars (ids, t_opt, seq_body) ->
      let types = match t_opt with Some t -> List.map (fun _ -> t) ids
        | None -> match seq_body with {idesc=Set(_, el);_}::_ -> (match el with [{edesc=Call(f,_);_}] -> (get_func_sig f.id).rets | _ -> List.flatten (List.map (get_expr_type env) el)) | _ -> List.map (fun _ -> TInt) ids in
      let new_env, alloc = List.fold_left2 (fun (e, code) id t -> let off = e.next_local in ({e with vars=StringMap.add id.id (off, t) e.vars; next_local=off-4}, code @@ li t0 0 @@ push t0)) (env, nop) ids types in
      alloc @@ tr_seq new_env seq_body
  | If(c, s1, s2) ->
      let l1 = new_label() in let l2 = new_label() in
      tr_expr env c @@ bnez t0 l1 @@ tr_seq env s2 @@ j l2 @@ label l1 @@ tr_seq env s1 @@ label l2
  | For(c, s) ->
      let l1 = new_label() in let l2 = new_label() in
      j l1 @@ label l2 @@ tr_seq env s @@ label l1 @@ tr_expr env c @@ bnez t0 l2
  | Block s -> tr_seq env s
  | Return el ->
      if env.ret_ptr_offsets <> [] then (match el with
        | [{edesc=Call(f, args);_}] when List.length (get_func_sig f.id).rets > 1 ->
            let push_args = List.fold_left (fun acc arg -> acc @@ tr_expr env arg @@ push t0) nop args in
            let pass_ptrs = List.fold_left (fun acc off -> acc @@ lw t0 off fp @@ push t0) nop env.ret_ptr_offsets in
            push_args @@ pass_ptrs @@ jal f.id @@ addi sp sp (4 * (List.length args + List.length env.ret_ptr_offsets)) @@ j env.exit_label
        | _ -> List.fold_left2 (fun acc expr off -> acc @@ tr_expr env expr @@ lw t1 off fp @@ sw t0 0 t1) nop el env.ret_ptr_offsets @@ j env.exit_label)
      else (match el with [] -> j env.exit_label | [e] -> tr_expr env e @@ move v0 t0 @@ j env.exit_label | _ -> exit 2)
  | Inc e -> tr_addr env e @@ push t0 @@ lw t0 0 t0 @@ addi t0 t0 1 @@ pop t1 @@ sw t0 0 t1
  | Dec e -> tr_addr env e @@ push t0 @@ lw t0 0 t0 @@ addi t0 t0 (-1) @@ pop t1 @@ sw t0 0 t1

(* --- Compilation des Fonctions et Programme --- *)

let tr_fun df =
  let sig_f = get_func_sig df.fname.id in
  let exit_lbl = "exit_" ^ df.fname.id in
  let n_imp = if List.length sig_f.rets > 1 then List.length sig_f.rets else 0 in
  let tot = List.length df.params + n_imp in
  let env_p, _ = List.fold_left (fun (e, i) (id, t) -> let off = 8 + 4*(tot-1-i) in ({e with vars=StringMap.add id.id (off, t) e.vars}, i+1)) ({empty_env with exit_label=exit_lbl}, 0) df.params in
  let env = if n_imp > 0 then let rec loop i acc = if i>=n_imp then (List.rev acc) else loop (i+1) ((8 + 4*(n_imp-1-i))::acc) in {env_p with ret_ptr_offsets=loop 0 []} else env_p in
  label df.fname.id @@ push ra @@ push fp @@ move fp sp @@ tr_seq env df.body @@ label exit_lbl @@ move sp fp @@ pop fp @@ pop ra @@ jr ra            

let tr_main df =
  let env = { empty_env with exit_label = "main_exit" } in
  label "main" @@ move fp sp @@ tr_seq env df.body @@ label "main_exit" @@ li v0 10 @@ syscall

let tr_prog decls =
  List.iter (function Struct s -> struct_table := StringMap.add s.sname.id (compute_struct_layout s) !struct_table | Fun f -> func_table := StringMap.add f.fname.id {nargs=List.length f.params; rets=f.return} !func_table) decls;
  let text = List.fold_left (fun code decl -> match decl with Fun f when f.fname.id="main" -> code @@ tr_main f | Fun f -> code @@ tr_fun f | _ -> code) nop decls in
  let printers = compile_struct_printers () in (* Ajout des printers *)
  let data = List.fold_left (fun acc (lbl, str) -> acc @@ label lbl @@ asciiz str) nop !strings in
  { text = text @@ printers; data = data }