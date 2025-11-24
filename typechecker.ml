open Mgoast

exception Error of location * string
let error loc msg = raise (Error (loc, msg))

(* Utilitaires d'affichage et d'erreur *)
let string_of_typ = function
  | TInt -> "int" | TBool -> "bool" | TString -> "string" | TStruct s -> "*" ^ s

let check loc expected actual =
  if expected <> actual then
    error loc (Printf.sprintf "expected %s, got %s" (string_of_typ expected) (string_of_typ actual))

(* Environnements *)
module StringMap = Map.Make(String)

type struct_info = { fields : typ StringMap.t; pos : location }
type func_info   = { params : typ list; rets : typ list; pos : location }

type env = {
  structs : struct_info StringMap.t;
  funcs   : func_info StringMap.t;
  vars    : typ StringMap.t;
  ret_ty  : typ list;
}

let empty_env = { structs = StringMap.empty; funcs = StringMap.empty; vars = StringMap.empty; ret_ty = [] }

let check_wf loc env = function
  | TStruct s when not (StringMap.mem s env.structs) -> error loc ("unknown struct: " ^ s)
  | _ -> ()

(* --- PASSE 1 : EXPRESSIONS --- *)

let get_binop_sig = function
  | Add | Sub | Mul | Div | Rem -> TInt, TInt
  | Lt | Le | Gt | Ge           -> TInt, TBool
  | And | Or                    -> TBool, TBool
  | Eq | Neq                    -> TInt, TBool

let rec type_expr env e =
  match e.edesc with
  | Int _ -> TInt | Bool _ -> TBool | String _ -> TString
  | Nil -> error e.eloc "nil needs context"
  | Var id ->
      (try StringMap.find id.id env.vars with Not_found -> error id.loc ("undefined var: " ^ id.id))
  | New s ->
      if not (StringMap.mem s env.structs) then error e.eloc ("unknown struct: " ^ s);
      TStruct s
  | Dot (e, id) ->
      (match type_expr env e with
       | TStruct s ->
           let fields = (StringMap.find s env.structs).fields in
           (try StringMap.find id.id fields with Not_found -> error id.loc ("no field " ^ id.id))
       | _ -> error e.eloc "dot access requires struct pointer")
  | Call (f, args) ->
      let info = try StringMap.find f.id env.funcs with Not_found -> error f.loc ("unknown func " ^ f.id) in
      check_args env f.loc info.params args;
      (match info.rets with [t] -> t | _ -> error e.eloc "func call must return exactly one value in expr")
  | Unop (op, e1) ->
      let t = type_expr env e1 in
      (match op, t with Opp, TInt -> TInt | Not, TBool -> TBool | _ -> error e.eloc "unop type mismatch")
  | Binop ((Eq | Neq), e1, e2) ->
      let t1, t2 = try type_expr env e1, type_expr env e2 with _ -> TInt, TInt in
      if e1.edesc = Nil && e2.edesc = Nil then error e.eloc "nil == nil illegal";
      if e1.edesc <> Nil && e2.edesc <> Nil && t1 <> t2 then error e.eloc "type mismatch in cmp";
      TBool
  | Binop (op, e1, e2) ->
      let t_in, t_out = get_binop_sig op in
      check e1.eloc t_in (type_expr env e1);
      check e2.eloc t_in (type_expr env e2);
      t_out
  | Print _ -> error e.eloc "Print is an instruction"

and check_args env loc expected args =
  if List.length expected <> List.length args then error loc "arity mismatch";
  List.iter2 (fun t arg ->
    if arg.edesc = Nil then (match t with TStruct _ -> () | _ -> error arg.eloc "nil requires struct type")
    else check arg.eloc t (type_expr env arg)
  ) expected args

(* --- PASSE 2 : INSTRUCTIONS --- *)

let check_lvalue e = match e.edesc with Var _ | Dot _ -> () | _ -> error e.eloc "lvalue required"

(* CORRECTION ICI : suppression de l'argument 'loc' et de la capture 'eloc' *)
let get_rhs_types env exprs =
  match exprs with
  | [{edesc = Call(f, args); _}] -> (* Cas spécial f() qui rend n valeurs *)
      let info = try StringMap.find f.id env.funcs with Not_found -> error f.loc "unknown func" in
      check_args env f.loc info.params args;
      info.rets
  | _ -> (* Cas standard: liste d'expressions *)
      List.map (fun e -> type_expr env e) exprs

let rec check_instr env instr =
  match instr.idesc with
  | Expr e ->
      (match e.edesc with
       | Print args ->
           List.iter (fun arg ->
             match arg.edesc with
             | Call (f, a) -> check_args env f.loc (StringMap.find f.id env.funcs).params a
             | Nil -> error arg.eloc "cannot print nil"
             | _ -> ignore (type_expr env arg)) args
       | _ -> ignore (type_expr env e))
  | Set (lvl, el) ->
      (* CORRECTION ICI : appel sans instr.iloc *)
      let types = get_rhs_types env el in
      if List.length lvl <> List.length types then error instr.iloc "assign arity mismatch";
      List.iter2 (fun lv t_ex ->
        check_lvalue lv;
        match lv.edesc with
        | Var {id="_";_} -> ()
        | _ -> check lv.eloc (type_expr env lv) t_ex
      ) lvl types
  | Vars (ids, t_opt, seq) ->
      let distinct = List.sort_uniq String.compare (List.map (fun i -> i.id) ids) in
      if List.length distinct <> List.length ids then error instr.iloc "duplicate var names";
      
      let new_vars = match t_opt, seq with
        | Some t, _ ->
            check_wf instr.iloc env t;
            List.fold_left (fun acc id -> StringMap.add id.id t acc) env.vars ids
        | None, {idesc=Set(_, el);_}::_ ->
            (* CORRECTION ICI : appel sans instr.iloc *)
            let types = get_rhs_types env el in
            if List.length ids <> List.length types then error instr.iloc "init arity mismatch";
            List.fold_left2 (fun acc id t -> if id.id="_" then acc else StringMap.add id.id t acc) env.vars ids types
        | _ -> error instr.iloc "invalid var decl"
      in
      check_seq { env with vars = new_vars } seq
  | Inc e | Dec e -> check_lvalue e; check e.eloc TInt (type_expr env e)
  | If (c, b1, b2) -> check c.eloc TBool (type_expr env c); check_seq env b1; check_seq env b2
  | For (c, b) -> check c.eloc TBool (type_expr env c); check_seq env b
  | Block b -> check_seq env b
  | Return el ->
      (* CORRECTION ICI : appel sans instr.iloc *)
      let types = get_rhs_types env el in
      if List.length types <> List.length env.ret_ty then error instr.iloc "return arity mismatch";
      List.iter2 (fun expected actual -> 
         if actual <> expected then check instr.iloc expected actual
      ) env.ret_ty types

and check_seq env s = List.iter (check_instr env) s

let rec returns_all seq =
  List.exists (fun i -> match i.idesc with Return _ -> true | If(_,b1,b2) -> returns_all b1 && returns_all b2 | Block b -> returns_all b | _ -> false) seq

(* --- MAIN --- *)

let prog (_, decls) =
  let structs = List.fold_left (fun acc -> function
    | Struct s -> 
        if StringMap.mem s.sname.id acc then error s.sname.loc "dup struct";
        StringMap.add s.sname.id {fields=StringMap.empty; pos=s.sname.loc} acc
    | _ -> acc) StringMap.empty decls in
  let env = { empty_env with structs } in

  let env = List.fold_left (fun e -> function
    | Fun f ->
        if StringMap.mem f.fname.id e.funcs then error f.fname.loc "dup func";
        List.iter (fun (_,t) -> check_wf f.fname.loc e t) f.params;
        List.iter (check_wf f.fname.loc e) f.return;
        { e with funcs = StringMap.add f.fname.id {params=List.map snd f.params; rets=f.return; pos=f.fname.loc} e.funcs }
    | Struct s ->
        let fields = List.fold_left (fun facc (id, t) ->
          check_wf id.loc e t;
          if StringMap.mem id.id facc then error id.loc "dup field";
          StringMap.add id.id t facc
        ) StringMap.empty s.fields in
        { e with structs = StringMap.add s.sname.id {fields; pos=s.sname.loc} e.structs }
  ) env decls in

  List.iter (function
    | Struct _ -> ()
    | Fun f ->
        let vars = List.fold_left (fun acc (id, t) -> StringMap.add id.id t acc) StringMap.empty f.params in
        check_seq { env with vars; ret_ty = f.return } f.body;
        if f.return <> [] && not (returns_all f.body) then error f.fname.loc "missing return"
  ) decls;
  
  if not (StringMap.mem "main" env.funcs) then 
    error (Lexing.dummy_pos, Lexing.dummy_pos) "main required";

  (* AJOUT : On renvoie les déclarations pour la suite de la compilation *)
  decls