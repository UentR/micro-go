open Mgoast

exception Error of Mgoast.location * string
let error loc s = raise (Error (loc,s))

let type_error loc ty_actual ty_expected =
  error loc (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)

(* 3 environnements pour stocker
     les variables avec leur type,
     les fonctions avec leur signature
     les structures avec leurs champs
*)
    
type tenv = typ Env.t
type fenv = (typ list) * (typ list) Env.t
type senv = (ident * typ) list

let dummy = "_"

let add_env l tenv =
  List.fold_left (fun env (x, t) -> if x = dummy then env else Env.add x t env) tenv l

let prog (fmt,ld) =
  (* collecte les noms des fonctions et des structures sans les vérifier *)
  let (fenv,senv) =
    List.fold_left
      (fun (fenv,senv) d ->
         match d with Struct(s) -> (fenv, Env.add s.sname.id s.fields senv)
                    | Fun(f)   -> let param_types = List.map snd f.params in (*AI*)
                      (Env.add f.fname.id (f.return, param_types) fenv, senv))(*AI*)
      (Env.empty, Env.empty) ld
  in
  let check_typ t = 
      match t with 
      | TInt | TBool | TString -> ()
      |TStruct s -> if not (Env.mem s senv) then error
           (Lexing.dummy_pos, Lexing.dummy_pos)
              ("Undefined struct: "^s)

  in
  let check_fields lf = List.iter (fun (_, t) -> check_typ t) lf
  in
  let rec check_expr e typ tenv =
    if e.edesc = Nil then  (match typ with
      |TStruct _ -> () 
      |_ -> type_error e.eloc (TStruct "") typ)

    else let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error e.eloc typ_e typ
  and type_expr e tenv = match e.edesc with
    | Int _  -> TInt
    | Bool _ -> TBool
    |String _ -> TString
    | Nil -> TStruct
    |Var x -> 
      if not (Env.mem x.id tenv) then error x.loc ("undefined variable: "^x.id)
      else Env.finf x.id tenv

    |Unop (Opp,e1) -> 
      check_expr e1 TInt tenv;
      TInt
    |Unop (Not, e1) -> 
      check_expr e1 TBool tenv;
      TBool

    | Binop((Add|Sub|Mul|Div|Rem), e1, e2) -> 
      check_expr e1 TInt tenv; 
      check_expr e2 TInt tenv;
      TInt
    |Binop((Lt|Le|Gt|Ge),e1,e2) -> 
      check_expr e1 TInt tenv;
      check_expr e2 TInt tenv;
      TBool
    |Binop((And|Or),e1,e2)-> 
      check_expr e1 TBool tenv; 
      check_expr e2 TBool tenv;
      TBool
    |Binop((Eq, Neq),e1,e2) ->
      check_expr e1 TBool tenv;
      check_expr e2 TBool tenv;
      TBool

    (*| Dot (e1, field) ->
      (match type_expr e1 tenv with
       | TStruct s ->
           if not (Env.mem s senv) then
             error e1.eloc ("undefined struct: " ^ s);
           let fields = Env.find s senv in
           (try List.assoc field.id (List.map (fun (i,t) -> (i.id, t)) fields)
            with Not_found -> 
              error field.loc ("struct " ^ s ^ " has no field " ^ field.id))
       | t -> type_error e1.eloc t (TStruct "..."))
    *)
    |New s -> if not (Env.mem s env) then error e.eloc ("Undefined struct:" ^s);
              TStruct s
    |Call (f, args) -> 
      if not (Env.mem f env) then error f.loc ("Undefined function"^f.id);
      let (return_types, param_types) = Env.find f.id fenv;
      if List.length args <> List.length param_types then error f.loc ("Not same length param args")
      (*On vérifie le type des arguments*)
      List.iter2 (fun argument type_param -> check_expr arguement type_parametre tenv) 
        args param_types;
      (match ret_types with
        | [t] -> t 
        | _ -> error f.loc ("function must return exactly one value"))
  in

  let rec check_instr i ret tenv = 
    match i.idesc with
    | Expr e -> let _ = type_expr e tenv in ()

    | Set(lvals,rvals) -> 
      if List.length lvals <> List.length rvals then 
        error i.iloc "wrong number of values in assignement";
      List.iter(fun lval rval ->
        let t_val = type_expr lval tenv in check_expr rval t_val tenv) lvals rvals

    | Inc e -> check_expr e TInt tenv
    | Dec e -> check_expr e TInt tenv

    | If(cond,then_block,else_block) -> 
        check_expr cond TBool tenv;
        check_seq then_block ret tenv;
        check_seq else_block ret tenv
    |For (e, b) -> 
      check_expr e TBool tenv;
      check_seq b ret tenv
    
    |Block b -> 
      check_seq b ret tenv

    |Return exprs -> 
      if List.length exprs <> List.length ret then 
        error i.iloc "wrong number of values";
      List.iter2 (fun e t -> check_expr e t tenv) exprs ret 

    |Vars (vars, typ_opt, b) -> 
      let new_tenv = match typ_opt with 
      |None -> tenv
      |Some t ->
        check_typ t;
        List.fold_left (fun env v -> Env.add v.id t env) tenv vars
      in
      check_seq body ret new_tenv

  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in
  
  let check_function f = 
    List.iter (fun (_, t) -> check_typ t) f.params;
    List.iter check_typ f.return;
    let tenv = add_env f.params Env.empty in
    check_seq f.body f.return tenv

  in Env.iter (fun _ lf -> check_fields lf) senv;
     Env.iter (fun _ fd -> check_function fd) fenv
