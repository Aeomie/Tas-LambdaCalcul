open Ast

(*Environnements de typage*)
type env = (string * typ) list
(*Listes d'équations*)
type equations = (typ * typ) list

(* memory cases*)
type region = string
type state = (region * term) list

(* generator of names of variables of types *)
let counter_var : int ref = ref 0
(* Compteur pour générer des régions fraîches *)
let region_counter = ref 0

let new_var () : string = 
  counter_var := !counter_var +1;
  "T" ^(string_of_int !counter_var)

let new_region_id () =
  region_counter := !region_counter + 1;
  "region" ^ (string_of_int !region_counter)

(* MUTUAL RECURSION: rename_var and free_vars *)
let rec rename_var (t:term) (old_v:string) (new_v:string) : term =
  match t with
    Var x -> if x = old_v then Var new_v else Var x
  | N n -> N n
  | Add (t1, t2) -> Add (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | Sub (t1, t2) -> Sub (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | App (t1, t2) -> App (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | Abs (x, t_body) -> 
      if x = old_v then Abs (x, t_body) 
      else Abs (x, rename_var t_body old_v new_v)
  | Hd t1 -> Hd (rename_var t1 old_v new_v)
  | Tl t1 -> Tl (rename_var t1 old_v new_v)
  | Cons (hd, tl) -> Cons (rename_var hd old_v new_v, rename_var tl old_v new_v)
  | IfZero (cond, then_e, else_e) ->
      IfZero (rename_var cond old_v new_v,
       rename_var then_e old_v new_v,
        rename_var else_e old_v new_v)
  | IfEmpty (cond, then_e, else_e) ->
      IfEmpty (rename_var cond old_v new_v,
       rename_var then_e old_v new_v,
        rename_var else_e old_v new_v)
  | Let (x, t1, t2) ->
      if x = old_v then Let (x, rename_var t1 old_v new_v, t2)
      else Let (x, rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | Fix (x, t1) ->
      if x = old_v then Fix (x, t1)
      else Fix (x, rename_var t1 old_v new_v)
  | Deref t1 -> Deref(rename_var t1 old_v new_v)
  | Ref t1 -> Ref(rename_var t1 old_v new_v)
  | Assign (t1, t2) -> Assign (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | Region _ | Unit | Nil -> t  (* For other terms like Unit, Nil, etc. *)


and free_vars (t:term) : string list =
  match t with
    Var x -> [x]
  | N n -> []
  | Add (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Sub (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | App (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Abs (x, t_body) -> List.filter (fun y -> y <> x) (free_vars t_body)
  | Hd t1 | Tl t1 | Deref t1 | Ref t1 | Fix (_, t1) -> free_vars t1
  | Cons (hd, tl) -> (free_vars hd) @ (free_vars tl)
  | IfZero (cond, then_e, else_e) | IfEmpty (cond, then_e, else_e) ->
      (free_vars cond) @ (free_vars then_e) @ (free_vars else_e)
  | Let (x, t1, t2) ->
      (free_vars t1) @ (List.filter (fun y -> y <> x) (free_vars t2))
  | Nil | Unit | Region _ -> []


and substitute_var (t:term) (v:string) (t0:term) : term =
  match t with
    Var x -> if x = v then t0 else Var x
  | N n -> N n
  | Add (t1, t2) -> Add (substitute_var t1 v t0, substitute_var t2 v t0)
  | Sub (t1, t2) -> Sub (substitute_var t1 v t0, substitute_var t2 v t0)
  | App (t1, t2) -> App (substitute_var t1 v t0, substitute_var t2 v t0)
  | Abs (x, t_body) -> 
      if x = v then Abs(x, t_body)
      else if List.mem x (free_vars t0) then
        let new_x : string = new_var () in
        let new_body : term = rename_var t_body x new_x in
        Abs (new_x, substitute_var new_body v t0)
      else 
        Abs (x, substitute_var t_body v t0)
  | Cons(hd, tl) -> Cons(substitute_var hd v t0, substitute_var tl v t0)
  | Hd t1 -> Hd (substitute_var t1 v t0)
  | Tl t1 -> Tl (substitute_var t1 v t0)
  | IfZero (cond, then_e, else_e) ->
      IfZero (substitute_var cond v t0,
       substitute_var then_e v t0,
        substitute_var else_e v t0)
  | IfEmpty (cond, then_e, else_e) ->
      IfEmpty (substitute_var cond v t0,
       substitute_var then_e v t0,
        substitute_var else_e v t0)
  | Let (x, t1, t2) ->
      if x = v then Let (x, substitute_var t1 v t0, t2)
      else Let (x, substitute_var t1 v t0, substitute_var t2 v t0)
  | Fix (x, t1) ->
      if x = v then Fix (x, t1)
      else Fix (x, substitute_var t1 v t0)
  | Deref e -> Deref (substitute_var e v t0)
  | Ref e -> Ref (substitute_var e v t0)
  | Assign (e1, e2) -> Assign (substitute_var e1 v t0, substitute_var e2 v t0)
  | Region r -> Region r
  | _ -> t  (* For other terms like Unit, Nil, etc. *)

let barendregt_checker(t:term) : term =
  let rec aux(t:term) (vars:string list) : term =
    match t with
      Var x -> Var x
    | N n -> N n
    | Add (t1, t2) -> Add (aux t1 vars, aux t2 vars)
    | App (t1, t2) -> App (aux t1 vars, aux t2 vars)
    | Abs (x, t_body) ->
      if List.mem x vars then 
        let new_x : string = new_var () in
        let new_body : term = rename_var t_body x new_x in
        Abs (new_x, aux new_body (new_x::vars))
      else
        Abs (x, aux t_body (x::vars))
  in aux t []

(* not sure about this , to recheck*)
let rec is_value (t:term) : bool =
  match t with
  | N _ -> true
  | Abs (_, _) -> true
  | Nil -> true
  | Unit -> true
  | Region _ -> true
  | Cons(v1, v2) -> is_value v1 && is_value v2
  | _ -> false


let rec can_reduce(t:term) : bool =
  match t with
  | Add(N _, N _) -> true
  | Add(t1, t2) -> can_reduce t1 || can_reduce t2
  | Sub(N _, N _) -> true
  | Sub(t1, t2) -> can_reduce t1 || can_reduce t2
  | App(Abs(_, _), v) when not (can_reduce v) -> true
  | App(t1, t2) -> can_reduce t1 || can_reduce t2
  | Let(_, _, _) -> true
  | Fix(_, _) -> true
  | IfZero(N 0, _, _) -> true
  | IfZero(N _, _, _) -> true
  | IfZero(cond, _, _) -> can_reduce cond
  | IfEmpty(Nil, _, _) -> true
  | IfEmpty(Cons(_, _), _, _) -> true
  | IfEmpty(cond, _, _) -> can_reduce cond
  
  (* LIST OPERATIONS - ADD THESE *)
  | Hd (Cons(_, _)) -> true
  | Hd t1 -> can_reduce t1
  | Tl (Cons(_, _)) -> true
  | Tl t1 -> can_reduce t1
  | Cons(t1, t2) -> can_reduce t1 || can_reduce t2
  
  | Ref e -> can_reduce e || is_value e 
  | Deref (Region _) -> true  (* can dereference a region *)
  | Deref e -> can_reduce e
  | Assign(Region _, v) when not (can_reduce v) -> true  (* ready to assign *)
  | Assign(e1, e2) -> can_reduce e1 || can_reduce e2
  
  (* VALUES - CANNOT REDUCE *)
  | Abs(_,_) | Var _ | N _ | Nil | Unit | Region _ -> false

let rec reduce_one_step ((t, st) : term * state) : term * state =
  match t with
  (* Direct redexes - state inchangé *)
  | Add(N n1, N n2) -> (N (n1 + n2), st)
  | Sub(N n1, N n2) -> (N (n1 - n2), st)
  | App(Abs(x, t_body), v) when not (can_reduce v) -> 
      (substitute_var t_body x v, st)
  
  (* Compound terms - propage le state *)
  | Add(t1, t2) -> 
      if can_reduce t1 then 
        let (t1', st') = reduce_one_step (t1, st) in
        (Add(t1', t2), st')
      else 
        let (t2', st') = reduce_one_step (t2, st) in
        (Add(t1, t2'), st')
  
  | Sub(t1, t2) -> 
      if can_reduce t1 then 
        let (t1', st') = reduce_one_step (t1, st) in
        (Sub(t1', t2), st')
      else 
        let (t2', st') = reduce_one_step (t2, st) in
        (Sub(t1, t2'), st')
  
  | App(t1, t2) -> 
      if can_reduce t1 then 
        let (t1', st') = reduce_one_step (t1, st) in
        (App(t1', t2), st')
      else 
        let (t2', st') = reduce_one_step (t2, st) in
        (App(t1, t2'), st')
  
  | Let(x, t1, t2) ->
      if can_reduce t1 then
        let (t1', st') = reduce_one_step (t1, st) in
        (Let(x, t1', t2), st')
      else
      (substitute_var t2 x t1, st)
  
  | Fix(x, t1) ->
      (substitute_var t1 x (Fix(x, t1)), st)
  
  | IfZero(N 0, then_e, else_e) ->
      (then_e, st)
  | IfZero(N _, then_e, else_e) -> 
      (else_e, st)
  | IfZero(cond, then_e, else_e) ->
      let (cond', st') = reduce_one_step (cond, st) in
      (IfZero(cond', then_e, else_e), st')
  
  | IfEmpty(Nil, then_e, else_e) ->
      (then_e, st)
  | IfEmpty(Cons(_, _), then_e, else_e) ->
      (else_e, st)
  | IfEmpty(cond, then_e, else_e) ->
      let (cond', st') = reduce_one_step (cond, st) in
      (IfEmpty(cond', then_e, else_e), st')
    (* LIST OPERATIONS *)
  | Hd t1 ->
      if can_reduce t1 then begin
        let (t1', st') = reduce_one_step (t1, st) in
        (Hd t1', st')
      end else begin
        (match t1 with
        | Cons(h, _) -> (h, st)
        | Nil -> failwith "hd of empty list"
        | _ -> (Hd t1, st))  (* stuck *)
      end
  
  | Tl t1 ->
      if can_reduce t1 then begin
        let (t1', st') = reduce_one_step (t1, st) in
        (Tl t1', st')
      end else begin
        (match t1 with
        | Cons(_, tl) -> (tl, st)
        | Nil -> failwith "tl of empty list"
        | _ -> (Tl t1, st))  (* stuck *)
      end
  
  | Cons(t1, t2) ->
      if can_reduce t1 then
        let (t1', st') = reduce_one_step (t1, st) in
        (Cons(t1', t2), st')
      else if can_reduce t2 then
        let (t2', st') = reduce_one_step (t2, st) in
        (Cons(t1, t2'), st')
      else
        (Cons(t1, t2), st)  (* both are values *)
  | Deref e -> 
      if can_reduce e then
        let (e', st') = reduce_one_step (e, st) in
        (Deref e', st')
      else
        (match e with
        | Region region_id ->
            (try
              let value = List.assoc region_id st in
              (value, st)
            with Not_found ->
              failwith ("Invalid region: " ^ region_id)
            )
        | _ -> failwith "Deref applied to non-region"
        )
  | Ref e -> 
      if can_reduce e then
        let (e', st') = reduce_one_step (e, st) in
        (Ref e', st')
      else
        if(is_value e) then
          let region_id = new_region_id () in
          let new_state = (region_id, e) :: st in
            Printf.printf "Created new region: %s with value\n" 
    region_id ;
          (Region region_id, new_state)
        else
          failwith "Ref applied to non-value"
  | Assign(e1, e2) ->
      if can_reduce e1 then
        let (e1', st') = reduce_one_step (e1, st) in
        (Assign(e1', e2), st')
      else if can_reduce e2 then
        let (e2', st') = reduce_one_step (e2, st) in
        (Assign(e1, e2'), st')
      else
        (match e1 with
        | Region region_id ->
            let new_state = (region_id, e2) :: (List.remove_assoc region_id st) in
            (Unit, new_state) (* Assignment returns Unit *)
        | _ -> failwith "Assign applied to non-region"
          )
  | _ -> (t, st)  (* No reduction possible *)

let left_right_eval_onestep ((t, st) : term * state) : term * state  =
  if can_reduce t then reduce_one_step (t, st) else (t, st)

exception VarPasTrouve

let rec print_term (t : term) : string = 
  match t with
    Var x -> x
  | N n -> string_of_int n
  | Add (t1, t2) -> "(" ^ (print_term t1) ^ " + " ^ (print_term t2) ^ ")"
  | Sub (t1, t2) -> "(" ^ (print_term t1) ^ " - " ^ (print_term t2) ^ ")"
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"
  | Nil -> "[]"
  | Unit -> "()"
  | Cons (hd, tl) -> "(" ^ (print_term hd) ^ " :: " ^ (print_term tl) ^ ")"
  | Hd t1 -> "hd(" ^ (print_term t1) ^ ")"
  | Tl t1 -> "tl(" ^ (print_term t1) ^ ")"
  | IfZero (t1, t2, t3) -> "ifzero "^ (print_term t1) ^ " then " ^ (print_term t2) ^ " else " ^ (print_term t3)
  | IfEmpty (t1, t2, t3) -> "ifempty "^ (print_term t1) ^ " then " ^ (print_term t2) ^ " else " ^ (print_term t3)
  | Let (x, t1, t2) -> "let " ^ x ^ " = " ^ (print_term t1) ^ " in " ^ (print_term t2)
  | Fix (x, t1) -> "fix (" ^ x ^ " -> " ^ (print_term t1) ^ ")" 
  | Deref t1 -> "!" ^ (print_term t1)
  | Ref t1 -> "ref " ^ (print_term t1)
  | Assign (t1, t2) -> (print_term t1) ^ " := " ^ (print_term t2)
  | Region r -> "region " ^ r
  

let rec search_type (v:string) (e:env) : typ =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::q -> if v1 = v then t1 else search_type v q

let rec search_variable_by_type (t:typ) (e:env) : string =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::q -> if t1 = t then v1 else search_variable_by_type t q


let rec belongs_type (v:string) (t:typ) : bool = 
  match t with
    Var v1 -> v1 = v
  | Arr (t1, t2) -> (belongs_type v t1) || (belongs_type v t2)
  | Nat -> false
  | List t1 -> belongs_type v t1
  | Forall (x, t1) -> if x = v then false else belongs_type v t1
  | UnitType -> false
  | RefType t1 -> belongs_type v t1
  | WeakVar v1 -> v1 = v


let rec substitue_type(t:typ) (v:string) (new_typ:typ) : typ =
  match t with
    Var v1 -> if v1 = v then new_typ else Var v1
  | Arr (t1, t2) -> Arr (substitue_type t1 v new_typ, substitue_type t2 v new_typ)
  | Nat -> Nat
  | List t1 -> List (substitue_type t1 v new_typ)
  | Forall (x, t1) ->
      if x = v then Forall (x, t1) (* Linked , Dont subtitute*)
      else Forall (x, substitue_type t1 v new_typ)
  | UnitType -> UnitType
  | RefType t1 -> RefType (substitue_type t1 v new_typ)
  | WeakVar v1 -> if v1 = v then new_typ else WeakVar v1


  (* gets the free vars from that type*)
let rec get_free_vars_type (t:typ) : string list =
  match t with
  Var x -> [x]
  | Nat -> []
  | Arr (t1, t2) -> (get_free_vars_type t1) @ (get_free_vars_type t2)
  | List t1 -> get_free_vars_type t1
  | Forall (x, t1) -> List.filter (fun y -> y <> x) (get_free_vars_type t1)
  | UnitType -> []
  | RefType t1 -> get_free_vars_type t1
  | WeakVar x -> [x]


  (* gets all the vars from the types in the env *)
let rec get_vars_types_env (e:env) : string list = 
  match e with
  [] -> []
  | (_, t):: rest -> (get_free_vars_type t) @ (get_vars_types_env rest)

let generalise (t:typ) (e:env) : typ =
  let type_vars = get_free_vars_type t in
  let env_vars = get_vars_types_env e in
  let vars_to_generalise = List.filter (fun x -> not (List.mem x env_vars)) type_vars in
  (*removes dupes if there are any *)
  let unique_vars = List.sort_uniq String.compare vars_to_generalise in
  List.fold_right (fun v acc -> Forall (v, acc)) unique_vars t

let rec weakify (t : typ) : typ =
  match t with
  | Var x -> WeakVar x
  | Arr(t1, t2) -> Arr(weakify t1, weakify t2)
  | List t1 -> List(weakify t1)
  | RefType t1 -> RefType(weakify t1)
  | Forall(x, t1) -> Forall(x, t1)   (* shouldn't happen *)
  | Nat -> Nat
  | UnitType -> UnitType
  | WeakVar x -> WeakVar x

let substitue_type_everywhere (e:equations) (v:string) (new_type: typ) : equations = 
  List.map (fun (x, y) -> (substitue_type x v new_type , substitue_type y v new_type)) e

exception Unif_fail of string

type equa_zip = equations * equations

let rec rewind (e: equa_zip) =
  match e with 
    ([], _) -> e
  | (c::e1, e2) -> rewind (e1, c::e2)

let substitue_type_zip (e: equa_zip) (v:string) (new_type: typ) : equa_zip =
  match e with
    (eq1, eq2) -> (substitue_type_everywhere eq1 v new_type, substitue_type_everywhere eq2 v new_type)

(* PRINTERS - needed before unification *)
let rec print_type (t : typ) : string =
  match t with
    Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
  | Nat -> "Nat"
  | UnitType -> "Unit"
  | RefType t1 -> "Ref[" ^ (print_type t1) ^ "]"
  | List t1 -> "List[" ^ (print_type t1) ^ "]"
  | Forall (v, t1) -> "∀" ^ v ^ ". " ^ (print_type t1)
  | WeakVar v -> "WeakVar " ^ v
  
let rec find_goal (e: equa_zip) (goal:string) : typ =
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (Var v, t)::q) when v = goal -> t
  | (_, (t, Var v)::q) when v = goal -> t
  | (e1, c::e2) -> find_goal (e1, e2) goal

  (*
  How zip works ( List of already processed equations , List of equations to process )
  *)

  let rec is_non_expansive (t: term) : bool =
  match t with
  | Var _ -> true           
  | N _ -> true          
  | Unit -> true            
  | Nil -> true            
  | Abs(_, _) -> true      
  
  (* Constructeurs purs *)
  | Cons(hd, tl) -> is_non_expansive hd && is_non_expansive tl
  
  (* Applications, Ref, Deref, Assign: PAS OK! *)
  | App(_, _) -> false       
  | Ref _ -> false           
  | Deref _ -> false           
  | Assign(_, _) -> false      
  
  (* Opérations *)
  | Add(t1, t2) -> is_non_expansive t1 && is_non_expansive t2
  | Sub(t1, t2) -> is_non_expansive t1 && is_non_expansive t2
  | Hd _ -> false          
  | Tl _ -> false            
  
  (* Branchements: dépend des sous-termes *)
  | IfZero(_, t1, t2) -> is_non_expansive t1 && is_non_expansive t2
  | IfEmpty(_, t1, t2) -> is_non_expansive t1 && is_non_expansive t2
  
  (* Let et Fix *)
  | Let(_, t1, t2) -> is_non_expansive t1 && is_non_expansive t2
  | Fix(_, _) -> false         
  
  | Region _ -> false    
  
let rec contains_forall t =
  match t with
  | Forall(_, _) -> true
  | Arr(t1, t2) -> contains_forall t1 || contains_forall t2
  | List t1 -> contains_forall t1
  | RefType t1 -> contains_forall t1
  | _ -> false


let rec generate_equations (te:term) (t:typ) (e:env) : equations = 
  match te with
    Var x -> let tv : typ = search_type x e in [(t,tv)]
  | N _ -> [(t, Nat)]
  | Unit -> [(t, UnitType)]
  | Add (t1, t2) ->
    let eq1 : equations = generate_equations t1 Nat e in
    let eq2 : equations = generate_equations t2 Nat e in
    (t, Nat) :: (eq1 @ eq2)
  | Sub (t1, t2) ->
    let eq1 : equations = generate_equations t1 Nat e in
    let eq2 : equations = generate_equations t2 Nat e in
    (t, Nat) :: (eq1 @ eq2)
  | App (t1, t2) ->
      let nv : string = new_var () in
      let eq1 = generate_equations t1 (Arr (Var nv, t)) e in
      let eq2 = generate_equations t2 (Var nv) e in
      eq1 @ eq2
  | Abs (x, t_body) ->
      let nv1 : string = new_var () 
      and nv2 : string = new_var () in
      (t, Arr (Var nv1, Var nv2)) ::
      (generate_equations t_body (Var nv2) ((x, Var nv1)::e))

  | Nil ->
      let nv : string = new_var () in
      [(t, List (Var nv))]
  | Cons (hd, tl) ->
      let nv : string = new_var () in
      let eq1 = generate_equations hd (Var nv) e in
      let eq2 = generate_equations tl (List (Var nv)) e in
      (t, List (Var nv)) :: (eq1 @ eq2)
  | Hd t1 ->
      let nv : string = new_var () in
      let eq1 = generate_equations t1 (List (Var nv)) e in
      (t, Var nv) :: eq1
  | Tl t1 ->
      let nv : string = new_var () in
      let eq1 = generate_equations t1 (List (Var nv)) e in
      (t, List (Var nv)) :: eq1

  | IfZero (cond, then_e, else_e) ->
      let eq1 = generate_equations cond Nat e in 
      let eq2 = generate_equations then_e t e in
      let eq3 = generate_equations else_e t e in
      eq1 @ eq2 @ eq3

  | IfEmpty(cond, then_e, else_e) ->
      let nv_elem : string = new_var () in
      let eq1 = generate_equations cond (List (Var nv_elem)) e in 
      let eq2 = generate_equations then_e t e in
      let eq3 = generate_equations else_e t e in
      eq1 @ eq2 @ eq3

  | Fix (phi, m) ->
      let nv : string = new_var () in
      let eq1 = generate_equations m (Var nv) ((phi, Var nv)::e) in
      (t, Var nv) :: eq1

| Let(x, term1, term2) ->
    let nv1 = new_var () in
    let eq1_list = generate_equations term1 (Var nv1) e in
    
    (* Unifier pour obtenir le type *)
    let eq1_zip = ([], eq1_list) in
    (try
      let t1 = unification eq1_zip nv1 in
      
      (* Généraliser ou pas *)
      let generalized_t1 = 
        if is_non_expansive term1 then
          generalise t1 e
        else
          t1  (* PAS de généralisation *)
      in
      
      (* Générer les équations pour term2 *)
      let eq2 = generate_equations term2 t ((x, generalized_t1)::e) in
      
      (* IMPORTANT: Retourner AUSSI les équations de term1 qui ne sont pas généralisées! *)
      if is_non_expansive term1 then
        eq2  (* Si généralisé, on peut oublier eq1 *)
      else
        eq1_list @ eq2  (* Sinon, garder les contraintes! *)
        
    with Unif_fail msg -> 
      raise (Unif_fail ("Erreur de typage dans let: " ^ msg)))
  | Ref t1 ->
      let nv = new_var () in
      let eq1 = generate_equations t1 (Var nv) e in
      (t, RefType (Var nv)) :: eq1   (* ← Var, pas WeakVar *)

  | Deref t1 ->
      let nv = new_var () in
      let eq1 = generate_equations t1 (RefType (Var nv)) e in
      (t, Var nv) :: eq1
  | Assign (t1, t2) ->
      let nv: string = new_var () in
      let eq1 = generate_equations t1 (RefType (Var nv)) e in
      let eq2 = generate_equations t2 (Var nv) e in
      (t, UnitType) :: (eq1 @ eq2)
  | Region r ->
      let nv: string = new_var () in
      [(t, RefType (Var nv))]  (* A region can be seen as a reference to some type *)

    
and unification (e : equa_zip) (but : string) : typ = 
  match e with 
  | (_, []) ->  (* reached the end *)
      (try find_goal (rewind e) but 
       with VarPasTrouve -> raise (Unif_fail "but pas trouvé"))
  | (e1, (t_left, t_right)::e2) -> 
      match (t_left, t_right) with
      (* same type *)
      | Nat , Nat -> unification (e1, e2) but
      | UnitType , UnitType -> unification (e1, e2) but
      (* Var cases *)
      | Var v1 , _ when v1 = but -> unification ((t_left, t_right)::e1, e2) but
      | Var v1, Var v2 ->
         unification (substitue_type_zip (rewind (e1,e2)) v2 (Var v1)) but
      | WeakVar w1, WeakVar w2 ->
        unification (substitue_type_zip (rewind (e1,e2)) w2 (WeakVar w1)) but
      (* For all*)
      | Forall(x, t1), t2 ->
          let fresh = new_var () in
          let t1_opened = substitue_type t1 x (Var fresh) in
          unification (e1, (t1_opened, t2)::e2) but
      
      | t1, Forall(x, t2) ->
          let fresh = new_var () in
          let t2_opened = substitue_type t2 x (Var fresh) in
          unification (e1, (t1, t2_opened)::e2) but
      (* List*)
      | List t1 , List t2 -> unification (e1, (t1, t2)::e2) but
      | RefType t1, RefType t2 -> unification (e1, (t1, t2)::e2) but
      
      (* if one of the two types are Var *)
      | Var v1 , t2 ->
        if belongs_type v1 t2 
        then raise (Unif_fail ("occurence de "^ v1 ^" dans "^(print_type t2)))
        else unification (substitue_type_zip (rewind (e1,e2)) v1 t2) but
      | t1, Var v2 ->
          if belongs_type v2 t1 
          then raise (Unif_fail ("occurence de "^ v2 ^" dans " ^(print_type t1))) 
          else unification (substitue_type_zip (rewind (e1,e2)) v2 t1) but
      (* Weak variable cases *)
      | WeakVar w1, t2 ->
          if belongs_type w1 t2 then
            raise (Unif_fail ("occurence de "^ w1 ^" dans "^(print_type t2)))
          else
            unification (substitue_type_zip (rewind (e1,e2)) w1 t2) but

      | t1, WeakVar w2 ->
          if belongs_type w2 t1 then
            raise (Unif_fail ("occurence de "^ w2 ^" dans "^(print_type t1)))
          else
            unification (substitue_type_zip (rewind (e1,e2)) w2 t1) but

      (* if they are arrow type *)
      | Arr (t1,t2), Arr (t3,t4) -> unification (e1, (t1, t3)::(t2, t4)::e2) but
      (* fail calls*)
      | RefType _ , _ -> raise (Unif_fail ("type référence non-unifiable avec "^(print_type t_right)))
      | _ , RefType _ -> raise (Unif_fail ("type référence non-unifiable avec "^(print_type t_left)))
      | UnitType , _ -> raise (Unif_fail ("type unité non-unifiable avec "^(print_type t_right)))
      | _ , UnitType -> raise (Unif_fail ("type unité non-unifiable avec "^(print_type t_left)))
      | Arr (_, _), _ -> raise (Unif_fail ("type fleche non-unifiable avec "^(print_type t_right)))
      | _, Arr (_, _) -> raise (Unif_fail ("type fleche non-unifiable avec "^(print_type t_left)))
      | List _ , _ -> raise (Unif_fail ("type liste non-unifiable avec "^(print_type t_right)))
      | _ , List _ -> raise (Unif_fail ("type liste non-unifiable avec "^(print_type t_left)))
      | Nat, t3 -> raise (Unif_fail ("type entier non-unifiable avec "^(print_type t3)))
      | t3, Nat -> raise (Unif_fail ("type entier non-unifiable avec "^(print_type t3)))

let inference (t: term) : string =
  let e : equa_zip = ([], generate_equations t (Var "goal") []) in
  try let res = unification e "goal" in
      (print_term t)^" ***TYPABLE*** avec le type "^(print_type res)
  with Unif_fail msg -> (print_term t)^" ***NOT TYPABLE*** : "^msg

let rec print_reductions (t, st) steps = 
  if steps = 0 then 
    print_endline (print_term t)
  else
    let (t', st') = left_right_eval_onestep (t, st) in
    if t' = t then 
      print_endline (print_term t)
    else begin
      print_endline (print_term t);
      print_reductions (t', st') (steps - 1)
    end
 
let () =
  if Array.length Sys.argv < 4 then (
    Printf.eprintf "Usage: %s <filename> <mode> [num_steps]\n" Sys.argv.(0);
    Printf.eprintf "  mode: 'type' for type inference, 'eval' for evaluation\n";
    Printf.eprintf "  num_steps: required for 'eval' mode\n";
    exit 1
  );

  let fname = Sys.argv.(1) in
  let mode = Sys.argv.(2) in
  let num_steps =
    try int_of_string Sys.argv.(3)
    with Failure _ ->
      Printf.eprintf "Third argument must be an integer\n";
      exit 1
  in

  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  try
    let prog_term = Parser.prog Lexer.token lexbuf in
    match mode with
    | "type" | "ty" ->
        let result = inference prog_term in
        print_endline result;
    | "eval" | "ev" ->
      print_reductions (prog_term, []) num_steps;
    | _ ->
        Printf.eprintf "Unknown mode: %s. Use 'type' or 'eval'.\n" mode;
    close_in ic;
    exit 1
  with
  | Lexer.Eof | Parsing.Parse_error ->
      Printf.eprintf "Error parsing file %s\n" fname;
      close_in ic;
      exit 1