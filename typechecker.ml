open Mgoast

(* Exception pour les erreurs de typage avec localisation *)
exception Error of location * string

(* Fonctions utilitaires d'erreur *)
let error loc msg = raise (Error (loc, msg))

let type_error loc expected actual =
  let string_of_typ = function
    | TInt -> "int"
    | TBool -> "bool"
    | TString -> "string"
    | TStruct s -> "*" ^ s
  in
  error loc (Printf.sprintf "expected type %s, but got %s" 
               (string_of_typ expected) (string_of_typ actual))

(* Environnements *)
module StringMap = Map.Make(String)

type struct_info = {
  fields : typ StringMap.t;
  pos : location; (* Pour rapporter les erreurs de déclaration *)
}

type func_info = {
  params : typ list;
  rets : typ list;
  pos : location;
}

type env = {
  structs : struct_info StringMap.t;
  funcs : func_info StringMap.t;
  vars : typ StringMap.t;
  current_func_ret : typ list; (* Pour vérifier les retours *)
}

let empty_env = {
  structs = StringMap.empty;
  funcs = StringMap.empty;
  vars = StringMap.empty;
  current_func_ret = [];
}

(* === UTILITAIRES DE TYPE === *)

(* Vérifie l'égalité des types.
   Note: TStruct "A" correspond au type pointeur *A du sujet.
   Deux pointeurs de structures sont compatibles s'ils pointent vers la même structure.
   'nil' n'a pas de type propre ici, il est traité contextuellement. *)
let check_type loc expected actual =
  if expected <> actual then type_error loc expected actual

(* Vérifie qu'un type est bien formé (référence une structure existante) *)
let check_type_wf loc env t =
  match t with
  | TStruct s -> 
      if not (StringMap.mem s env.structs) then
        error loc ("unknown structure type: " ^ s)
  | _ -> ()

(* === PASSE 1 : VERIFICATION DES EXPRESSIONS === *)

(* Retourne le type d'une expression *)
let rec expr_type env e =
  match e.edesc with
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
  | Nil -> error e.eloc "nil cannot be used without context (explicit typing required)"
  
  | Var id ->
      begin try StringMap.find id.id env.vars
      with Not_found -> error id.loc ("undefined variable: " ^ id.id)
      end

  | Dot (obj, field) ->
      let t_obj = expr_type env obj in
      begin match t_obj with
      | TStruct sname ->
          let s_info = StringMap.find sname env.structs in
          begin try StringMap.find field.id s_info.fields
          with Not_found -> error field.loc ("structure " ^ sname ^ " has no field " ^ field.id)
          end
      | _ -> error obj.eloc "type mismatch: expected a structure pointer for field access"
      end

  | New sname ->
      if not (StringMap.mem sname env.structs) then
        error e.eloc ("unknown structure: " ^ sname);
      TStruct sname

  | Call (fname, args) ->
      let f_info = 
        try StringMap.find fname.id env.funcs
        with Not_found -> error fname.loc ("undefined function: " ^ fname.id)
      in
      check_args env fname.loc f_info.params args;
      begin match f_info.rets with
      | [t] -> t
      | [] -> error e.eloc "function does not return a value"
      | _ -> error e.eloc "function returns multiple values, use it in assignment"
      end

  | Unop (op, e1) ->
      let t1 = expr_type env e1 in
      begin match op, t1 with
      | Opp, TInt -> TInt
      | Not, TBool -> TBool
      | _, _ -> error e.eloc "unary operator type mismatch"
      end

  | Binop (op, e1, e2) ->
      (* Cas spécial pour nil dans les comparaisons (==, !=) *)
      begin match op, e1.edesc, e2.edesc with
      | (Eq | Neq), Nil, Nil -> error e.eloc "cannot compare nil with nil"
      | (Eq | Neq), _, Nil ->
          let t1 = expr_type env e1 in
          begin match t1 with
          | TStruct _ -> TBool
          | _ -> error e1.eloc "only structures can be compared to nil"
          end
      | (Eq | Neq), Nil, _ ->
          let t2 = expr_type env e2 in
          begin match t2 with
          | TStruct _ -> TBool
          | _ -> error e2.eloc "only structures can be compared to nil"
          end
      | _ ->
          let t1 = expr_type env e1 in
          let t2 = expr_type env e2 in
          match op with
          | Add | Sub | Mul | Div | Rem ->
              check_type e1.eloc TInt t1;
              check_type e2.eloc TInt t2;
              TInt
          | Lt | Le | Gt | Ge ->
              check_type e1.eloc TInt t1;
              check_type e2.eloc TInt t2;
              TBool
          | And | Or ->
              check_type e1.eloc TBool t1;
              check_type e2.eloc TBool t2;
              TBool
          | Eq | Neq ->
              if t1 <> t2 then error e.eloc "type mismatch in comparison";
              (* On peut comparer int, bool, string, et pointeurs *)
              TBool
      end

  (* Print n'est pas une expression valide (c'est une instruction) mais 
     l'AST le permet parfois via Expr. Ici on vérifie qu'on ne l'utilise pas dans un calcul *)
  | Print _ -> error e.eloc "fmt.Print cannot be used as an expression"

and check_args env loc expected_types args =
  if List.length expected_types <> List.length args then
    error loc "wrong number of arguments";
  List.iter2 (fun t_expected arg ->
    (* Gestion spéciale de nil passé en argument *)
    match arg.edesc with
    | Nil -> 
        begin match t_expected with
        | TStruct _ -> ()
        | _ -> error arg.eloc "nil cannot be passed for non-pointer argument"
        end
    | _ -> 
        let t_arg = expr_type env arg in
        check_type arg.eloc t_expected t_arg
  ) expected_types args

(* === PASSE 2 : VERIFICATION DES INSTRUCTIONS === *)

(* Vérifie si une expression est une "l-value" (assignable) *)
let check_lvalue e =
  match e.edesc with
  | Var _ | Dot _ -> ()
  | _ -> error e.eloc "expression is not assignable (must be variable or field)"

(* Vérifie si une suite d'instructions se termine par un return (pour le contrôle de flux) *)
let rec returns_on_all_paths seq =
  match seq with
  | [] -> false
  | i :: rest ->
      match i.idesc with
      | Return _ -> true
      | If (_, b1, b2) -> 
          if returns_on_all_paths b1 && returns_on_all_paths b2 then true
          else returns_on_all_paths rest
      | Block b -> 
          if returns_on_all_paths b then true 
          else returns_on_all_paths rest
      (* Vars contient la suite du bloc, c'est crucial *)
      | Vars (_, _, s) -> returns_on_all_paths s
      | For (_, _) -> returns_on_all_paths rest (* CORRECTION : Suppression de 'b' inutilisé *)
      | _ -> returns_on_all_paths rest

let rec check_instr env instr =
  match instr.idesc with
  (* On traite les expressions utilisées comme instructions *)
  | Expr e -> 
      begin match e.edesc with
      (* Cas spécial : fmt.Print(...) est stocké comme une expression Print ici *)
      | Print args ->
          let check_print_arg arg =
             match arg.edesc with
             | Call (fname, cargs) ->
                 (* Cas spécial: fmt.Print(f()) où f retourne plusieurs valeurs *)
                 let f_info = try StringMap.find fname.id env.funcs with Not_found -> error fname.loc "unknown func" in
                 check_args env fname.loc f_info.params cargs
                 (* On accepte n'importe quoi en retour pour Print *)
             | Nil -> error arg.eloc "cannot print nil"
             | _ -> ignore (expr_type env arg)
          in
          List.iter check_print_arg args
      
      (* Autres expressions (ex: appels de fonctions void) *)
      | _ -> ignore (expr_type env e)
      end
  
  (* Le cas | Print args -> ... a été supprimé car il n'existe pas dans instr_desc *)

  | Set (lvl, el) ->
      List.iter check_lvalue lvl;
      
      (* Cas spécial: affectation d'un appel de fonction multiple : x, y = f() *)
      if List.length el = 1 && List.length lvl > 1 then begin
        match (List.hd el).edesc with
        | Call(fname, args) ->
            let f_info = try StringMap.find fname.id env.funcs with Not_found -> error fname.loc "unknown func" in
            check_args env fname.loc f_info.params args;
            if List.length f_info.rets <> List.length lvl then
              error instr.iloc "arity mismatch in assignment";
            List.iter2 (fun lv expected_typ ->
               match lv.edesc with
               | Var {id="_"; _} -> () (* Ignorer _ *)
               | _ -> check_type lv.eloc (expr_type env lv) expected_typ
            ) lvl f_info.rets
        | _ -> error instr.iloc "arity mismatch in assignment"
      end else begin
        (* Cas standard: x, y = e1, e2 *)
        if List.length lvl <> List.length el then
          error instr.iloc "arity mismatch in assignment";
        List.iter2 (fun lv e ->
          match lv.edesc with
          | Var {id="_"; _} -> 
              if e.edesc = Nil then error e.eloc "cannot assign nil to _ without context";
              ignore (expr_type env e)
          | _ ->
             let t_lv = expr_type env lv in
             match e.edesc with
             | Nil -> 
                 begin match t_lv with
                 | TStruct _ -> ()
                 | _ -> error e.eloc "cannot assign nil to non-pointer"
                 end
             | _ -> check_type e.eloc t_lv (expr_type env e)
        ) lvl el
      end

  | Vars (ids, typ_opt, seq) ->
      (* 1. Vérifier que les identifiants sont distincts *)
      let names = List.map (fun id -> id.id) ids in
      if List.length (List.sort_uniq String.compare names) <> List.length names then
        error instr.iloc "duplicate variable names in declaration";

      (* 2. Calcul des nouvelles variables dans l'environnement *)
      let new_vars_env = 
        match typ_opt with
        | Some t ->
            (* CAS 1 : Type explicite (var x int) *)
            List.fold_left (fun acc_vars id ->
              check_type_wf id.loc env t;
              if id.id = "_" then acc_vars else StringMap.add id.id t acc_vars
            ) env.vars ids

        | None ->
            (* CAS 2 : Inférence de type (var x = e  OU  x := e) *)
            (* On regarde la première instruction de la séquence pour trouver l'initialisation *)
            match seq with
            | {idesc=Set(lvl, el); _} :: _ ->
                 (* Inférence des types basés sur 'el' *)
                 let inferred_types = 
                   if List.length el = 1 && List.length lvl > 1 then
                     (* Cas f() retournant plusieurs valeurs *)
                     match (List.hd el).edesc with
                     | Call(fname, _) ->
                         let f = try StringMap.find fname.id env.funcs with Not_found -> error fname.loc "unknown func" in
                         f.rets
                     | _ -> error instr.iloc "cannot infer types from expression"
                   else
                     (* Cas e1, e2 *)
                     List.map (expr_type env) el
                 in
                 
                 if List.length ids <> List.length inferred_types then
                    error instr.iloc "arity mismatch in initialization";
                 
                 List.fold_left2 (fun acc id t ->
                   if id.id = "_" then acc else StringMap.add id.id t acc
                 ) env.vars ids inferred_types
            
            | _ -> error instr.iloc "variable declaration needs explicit type or initialization"
      in

      (* On continue le typage de la séquence avec le nouvel environnement *)
      check_block { env with vars = new_vars_env } seq

  | Inc e | Dec e ->
      check_lvalue e;
      check_type e.eloc TInt (expr_type env e)

  | If (cond, b1, b2) ->
      check_type cond.eloc TBool (expr_type env cond);
      check_block env b1;
      check_block env b2

  | For (cond, b) -> (* CORRECTION : on remplace 'b' par '_' car on ne l'utilise pas ici *)
      check_type cond.eloc TBool (expr_type env cond);
      check_block env b

  | Return el ->
      if List.length el <> List.length env.current_func_ret then
        error instr.iloc "wrong number of return values";
      List.iter2 (fun expected e ->
        match e.edesc with
        | Nil -> 
             begin match expected with
             | TStruct _ -> ()
             | _ -> error e.eloc "cannot return nil as non-pointer"
             end
        | _ -> check_type e.eloc expected (expr_type env e)
      ) env.current_func_ret el

  | Block b -> check_block env b

and check_block env seq =
  List.iter (check_instr env) seq

(* === PROGRAMME PRINCIPAL === *)

let prog (_, decls) =
  
  (* --- ETAPE 1 : Collecter les structures (pour gérer la récursivité) --- *)
  let structs_init = List.fold_left (fun acc d ->
    match d with
    | Struct s ->
        if StringMap.mem s.sname.id acc then
          error s.sname.loc ("duplicate structure name: " ^ s.sname.id);
        StringMap.add s.sname.id { fields = StringMap.empty; pos = s.sname.loc } acc
    | _ -> acc
  ) StringMap.empty decls in

  let env_pass1 = { empty_env with structs = structs_init } in

  (* --- ETAPE 2 : Collecter les champs et les fonctions --- *)
  let env_pass2 = List.fold_left (fun env d ->
    match d with
    | Struct s ->
        (* Vérification des champs *)
        let fields = List.fold_left (fun acc (id, t) ->
          check_type_wf id.loc env t;
          if StringMap.mem id.id acc then
            error id.loc ("duplicate field name: " ^ id.id);
          StringMap.add id.id t acc
        ) StringMap.empty s.fields in
        let s_info = { fields = fields; pos = s.sname.loc } in
        { env with structs = StringMap.add s.sname.id s_info env.structs }
    
    | Fun f ->
        if StringMap.mem f.fname.id env.funcs then
           error f.fname.loc ("duplicate function name: " ^ f.fname.id);
        (* Vérif types params et retours *)
        List.iter (fun (_, t) -> check_type_wf f.fname.loc env t) f.params;
        List.iter (check_type_wf f.fname.loc env) f.return;
        (* Vérif duplicité params *)
        let param_names = List.map (fun (id,_) -> id.id) f.params in
        if List.length (List.sort_uniq String.compare param_names) <> List.length param_names then
           error f.fname.loc "duplicate parameter names";
        
        let f_info = { 
          params = List.map snd f.params; 
          rets = f.return; 
          pos = f.fname.loc 
        } in
        { env with funcs = StringMap.add f.fname.id f_info env.funcs }
  ) env_pass1 decls in

  (* --- ETAPE 3 : Vérifier les corps des fonctions --- *)
  List.iter (fun d ->
    match d with
    | Struct _ -> ()
    | Fun f ->
        (* Construire l'env local avec les paramètres *)
        let local_vars = List.fold_left (fun acc (id, t) ->
          StringMap.add id.id t acc
        ) StringMap.empty f.params in
        
        let func_env = { env_pass2 with 
          vars = local_vars; 
          current_func_ret = f.return 
        } in
        
        (* Vérification des instructions *)
        check_block func_env f.body;

        (* Vérification du chemin de retour si la fonction retourne qqch *)
        if f.return <> [] then
          if not (returns_on_all_paths f.body) then
            error f.fname.loc "function might not return"
  ) decls;

  (* --- VERIFICATIONS FINALES --- *)
  (* Vérifier presence main *)
  begin try
    let main = StringMap.find "main" env_pass2.funcs in
    if main.params <> [] || main.rets <> [] then
      error main.pos "main function must have no parameters and no return values"
  with Not_found ->
    (* On prend la loc du début du fichier faute de mieux, ou une loc dummy *)
    raise (Error ((Lexing.dummy_pos, Lexing.dummy_pos), "missing main function"))
  end;

  (* On pourrait vérifier l'usage de fmt ici si besoin, mais le parser le fait déjà partiellement *)
  ()