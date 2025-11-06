(*Terms*)
type term = 
  Var of string (* Variable *)
  | N of int (* Nombre *)
  | Add of term * term (* Addition *)
  | App of term * term (* Application *)
  | Abs of string * term (* Abstraction *)

(*Types*)
type typ = 
  Var of string (* Variable de type *)
  | Arr of typ * typ (* flèche *)
  | Nat (* Type des naturels *)

(*Environnements de typage*)
type env = (string * typ) list
(*Listes d'équations*)
type equations = (typ * typ) list

(* generator of names of variables of types *)
let counter_var : int ref = ref 0

let new_var () : string = 
  counter_var := !counter_var +1;
  "T" ^(string_of_int !counter_var)

(* MUTUAL RECURSION: rename_var and free_vars *)
let rec rename_var (t:term) (old_v:string) (new_v:string) : term =
  match t with
    Var x -> if x = old_v then Var new_v else Var x
  | N n -> N n
  | Add (t1, t2) -> Add (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | App (t1, t2) -> App (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | Abs (x, t_body) -> 
      if x = old_v then Abs (x, t_body) 
      else Abs (x, rename_var t_body old_v new_v)

and free_vars (t:term) : string list =
  match t with
    Var x -> [x]
  | N n -> []
  | Add (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | App (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Abs (x, t_body) -> List.filter (fun y -> y <> x) (free_vars t_body)

and substitute_var (t:term) (v:string) (t0:term) : term =
  match t with
    Var x -> if x = v then t0 else Var x
  | N n -> N n
  | Add (t1, t2) -> Add (substitute_var t1 v t0, substitute_var t2 v t0)
  | App (t1, t2) -> App (substitute_var t1 v t0, substitute_var t2 v t0)
  | Abs (x, t_body) -> 
      if x = v then Abs(x, t_body)
      else if List.mem x (free_vars t0) then
        let new_x : string = new_var () in
        let new_body : term = rename_var t_body x new_x in
        Abs (new_x, substitute_var new_body v t0)
      else 
        Abs (x, substitute_var t_body v t0)

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

let rec can_reduce(t:term) : bool =
  match t with
  | Add(N _, N _) -> true
  | Add(t1, t2) -> can_reduce t1 || can_reduce t2
  | App(Abs(_, _), (N _ | Abs _)) -> true
  | App(t1, t2) -> can_reduce t1 || can_reduce t2
  | Abs(_,_) -> false
  | Var _ -> false
  | N _ -> false

let rec reduce_one_step(t:term) : term =
  match t with
  (* Direct redexes - reduce them *)
  | Add(N n1, N n2) -> N (n1 + n2)
  | App(Abs(x, t_body), (N _ | Abs _ as v)) -> substitute_var t_body x v
  
  (* Compound terms - find leftmost redex *)
  | Add(t1, t2) -> 
      if can_reduce t1 then 
        Add(reduce_one_step t1, t2)  (* Still recursively finds leftmost in t1 *)
      else 
        Add(t1, reduce_one_step t2)
  
  | App(t1, t2) -> 
      if can_reduce t1 then 
        App(reduce_one_step t1, t2)
      else 
        App(t1, reduce_one_step t2)
  
  (* Already in normal form *)
  | _ -> t

let left_right_eval_onestep (t:term) : term =
  if can_reduce t then reduce_one_step t else t

exception VarPasTrouve

let rec search_type (v:string) (e:env) : typ =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::q -> if v1 = v then t1 else search_type v q

let rec belongs_type (v:string) (t:typ) : bool = 
  match t with
    Var v1 -> v1 = v
  | Arr (t1, t2) -> (belongs_type v t1) || (belongs_type v t2)
  | Nat -> false

let rec substitue_type(t:typ) (v:string) (new_typ:typ) : typ =
  match t with
    Var v1 -> if v1 = v then new_typ else Var v1
  | Arr (t1, t2) -> Arr (substitue_type t1 v new_typ, substitue_type t2 v new_typ)
  | Nat -> Nat

let substitue_type_everywhere (e:equations) (v:string) (new_type: typ) : equations = 
  List.map (fun (x, y) -> (substitue_type x v new_type , substitue_type y v new_type)) e

let rec generate_equations (te:term) (t:typ) (e:env) : equations = 
  match te with
    Var x -> let tv : typ = search_type x e in [(t,tv)]
  | N _ -> [(t, Nat)]
  | Add (t1, t2) 
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

let rec print_term (t : term) : string = 
  match t with
    Var x -> x
  | N n -> string_of_int n
  | Add (t1, t2) -> "(" ^ (print_term t1) ^ " + " ^ (print_term t2) ^ ")"
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"

let rec find_goal (e: equa_zip) (goal:string) : typ =
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (Var v, t)::q) when v = goal -> t
  | (_, (t, Var v)::q) when v = goal -> t
  | (e1, c::e2) -> find_goal (e1, e2) goal

let rec unification (e : equa_zip) (but : string) : typ = 
  match e with 
  | (_, []) -> 
      (try find_goal (rewind e) but 
       with VarPasTrouve -> raise (Unif_fail "but pas trouvé"))
  | (e1, (t_left, t_right)::e2) -> 
      match (t_left, t_right) with
      | Var v1, _ when v1 = but -> unification ((t_left, t_right)::e1, e2) but
      | Var v1, Var v2 -> unification (substitue_type_zip (rewind (e1,e2)) v2 (Var v1)) but
      | Var v1, t2 -> 
          if belongs_type v1 t2 
          then raise (Unif_fail ("occurence de "^ v1 ^" dans "^(print_type t2))) 
          else unification (substitue_type_zip (rewind (e1,e2)) v1 t2) but
      | t1, Var v2 ->
          if belongs_type v2 t1 
          then raise (Unif_fail ("occurence de "^ v2 ^" dans " ^(print_type t1))) 
          else unification (substitue_type_zip (rewind (e1,e2)) v2 t1) but
      | Arr (t1,t2), Arr (t3,t4) -> unification (e1, (t1, t3)::(t2, t4)::e2) but
      | Arr (_, _), _ -> raise (Unif_fail ("type fleche non-unifiable avec "^(print_type t_right)))
      | _, Arr (_, _) -> raise (Unif_fail ("type fleche non-unifiable avec "^(print_type t_left)))
      | Nat, Nat -> unification (e1, e2) but
      | Nat, t3 -> raise (Unif_fail ("type entier non-unifiable avec "^(print_type t3)))
      | t3, Nat -> raise (Unif_fail ("type entier non-unifiable avec "^(print_type t3)))

let inference (t: term) : string =
  let e : equa_zip = ([], generate_equations t (Var "goal") []) in
  try let res = unification e "goal" in
      (print_term t)^" ***TYPABLE*** avec le type "^(print_type res)
  with Unif_fail msg -> (print_term t)^" ***NOT TYPABLE*** : "^msg

let rec print_reductions t steps = 
  if steps = 0 then 
    print_endline (print_term t)
  else
    let t' = left_right_eval_onestep t in
    if t' = t then 
      print_endline (print_term t)
    else begin
      print_endline (print_term t);
      print_reductions t' (steps - 1)
    end
 

(* ***EXEMPLES*** *)
let ex_add : term = Add (N 3, N 5)
let () = print_endline "=== Example 1: 3 + 5 ==="; print_reductions ex_add 5; print_endline ""

let ex_lambda_add : term = App (Abs ("x", Add (Var "x", N 1)), N 7)
let () = print_endline "=== Example 2: (fun x -> x + 1) 7 ==="; print_reductions ex_lambda_add 5; print_endline ""

let ex_higher_order : term =
  App (
    Abs ("f", Add (App (Var "f", N 2), App (Var "f", N 3))),
    Abs ("x", Add (Var "x", N 10))
  )
let () = print_endline "=== Example 3: (fun f -> f 2 + f 3) (fun x -> x + 10) ==="; print_reductions ex_higher_order 10; print_endline ""

let main () = ()

let _ = main ()