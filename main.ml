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

(*pretty printer de termes*)
let rec print_term (t : term) : string = 
  match t with
    Var x -> x
  | N n -> string_of_int n
  | Add (t1, t2) -> "(" ^ (print_term t1) ^ " + " ^ (print_term t2) ^ ")"
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"

(*pretty printer of types*)
let rec print_type (t : typ) : string =
  match t with
    Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
  | Nat -> "Nat"

(* generator of names of variables of types *)
let counter_var : int ref = ref 0

let new_var () : string = 
  counter_var := !counter_var +1;
  "T" ^(string_of_int !counter_var)

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
  in aux t [];;

and rename_var (t:term) (old_v:string) (new_v:string) : term =
  match t with
    Var x -> if x = old_v then Var new_v else Var x
  | N n -> N n
  | Add (t1, t2) -> Add (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | App (t1, t2) -> App (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | Abs (x, t_body) -> 
      if x = old_v then Abs (x, t_body) 
      else Abs (x, rename_var t_body old_v new_v)

exception VarPasTrouve



(* looks for the type of a variable in an environment *)
let rec search_type (v:string) (e:env) : typ =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::q -> if v1 = v then t1 else search_type v q

(* checker of occurrence of variables *)
let rec belongs_type (v:string) (t:typ) : bool = 
  match t with
    Var v1 -> v1 = v
  | Arr (t1, t2) -> (belongs_type v t1) || (belongs_type v t2)
  | Nat -> false

(* replaces a variable by a type in type *)
let rec substitue_type(t:typ) (v:string) (new_typ:typ) : typ =
  match t with
    Var v1 -> if v1 = v then new_typ else Var v1
  | Arr (t1, t2) -> Arr (substitue_type t1 v new_typ, substitue_type t2 v new_typ)
  | Nat -> Nat

(* replaces a variable by a type in a list of equations *)
let substitue_type_everywhere (e:equations) (v:string) (new_type: typ) : equations = 
  List.map (fun (x, y) -> (substitue_type x v new_type , substitue_type y v new_type)) e


(**)(* replaces a variable by a term in a term *)
let rec rename_var (t:term) (old_v:string) (new_v:string) : term =
  match t with
    Var x -> when x = old_v -> Var new_v
  | N n -> N n
  | Add (t1, t2) -> Add (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | App (t1, t2) -> App (rename_var t1 old_v new_v, rename_var t2 old_v new_v)
  | Abs (x, t_body) -> 
      if x = old_v then Abs (x, t_body) 
      else Abs (x, rename_var t_body old_v new_v)

(* replaces a variable by a term in a term *)
(* generates typing equations from a term *)
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

(* equations zipper *)
type equa_zip = equations * equations

(* rewinds the zipper *)
(* is written as like , (functions_processed, functions not yet processed)*)
let rec rewind (e: equa_zip) =
  match e with 
    ([], _) -> e
  | (c::e1, e2) -> (e1, c::e2)

(* replaces a variable by a type in an equations zipper *)
let substitue_type_zip (e: equa_zip) (v:string) (new_type: typ) : equa_zip =
  match e with
    (eq1, eq2) -> (substitue_type_everywhere eq1 v new_type, substitue_type_everywhere eq2 v new_type)

(* find a type associated to variable in a zipper equation *)
let rec find_goal (e: equa_zip) (goal:string) : typ =
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (Var v, t)::q) when v =  goal -> t
  | (_, (t, Var v)::q) when v = goal -> t
  | (e1, c::e2) -> find_goal (e1, e2) goal

(* unification algorithm *)
let rec unification (e : equa_zip) (but : string) : typ = 
  match e with 
  | (_, []) -> 
      (try find_goal (rewind e) but 
       with VarPasTrouve -> raise (Unif_fail "but pas trouvé"))
  | (e1, (t_left, t_right)::e2) -> 
      match (t_left, t_right) with
      (* equation avec but à gauche *)
      | Var v1, _ when v1 = but -> unification ((t_left, t_right)::e1, e2) but
      (* deux variables : remplacer l'une par l'autre *)
      | Var v1, Var v2 -> unification (substitue_type_zip (rewind (e1,e2)) v2 (Var v1)) but
      (* une variable à gauche : vérification d'occurence puis remplacement *)
      | Var v1, t2 -> 
          if belongs_type v1 t2 
          then raise (Unif_fail ("occurence de "^ v1 ^" dans "^(print_type t2))) 
          else unification (substitue_type_zip (rewind (e1,e2)) v1 t2) but
      (* une variable à droite : vérification d'occurence puis remplacement *)
      | t1, Var v2 ->
          if belongs_type v2 t1 
          then raise (Unif_fail ("occurence de "^ v2 ^" dans " ^(print_type t1))) 
          else unification (substitue_type_zip (rewind (e1,e2)) v2 t1) but
      (* types flèche des deux côtés : on decompose *)
      | Arr (t1,t2), Arr (t3,t4) -> unification (e1, (t1, t3)::(t2, t4)::e2) but
      (* types flèche à gauche pas à droite : échec *)
      | Arr (_, _), _ -> raise (Unif_fail ("type fleche non-unifiable avec "^(print_type t_right)))
      (* types flèche à droite pas à gauche : échec *)
      | _, Arr (_, _) -> raise (Unif_fail ("type fleche non-unifiable avec "^(print_type t_left)))
      (* types nat des deux côtés : on passe *)
      | Nat, Nat -> unification (e1, e2) but
      (* types nat à gauche pas à droite : échec *)
      | Nat, t3 -> raise (Unif_fail ("type entier non-unifiable avec "^(print_type t3)))
      (* types nat à droite pas à gauche : échec *)
      | t3, Nat -> raise (Unif_fail ("type entier non-unifiable avec "^(print_type t3)))


(* chain generation of equation and unification *)
let inference (t: term) : string =
  let e : equa_zip = ([], generate_equations t (Var "goal") []) in
  try let res = unification e "goal" in
      (print_term t)^" ***TYPABLE*** avec le type "^(print_type res)
  with Unif_fail msg -> (print_term t)^" ***NOT TYPABLE*** : "^msg

(* ***EXEMPLES*** *)  
let ex_id : term = Abs ("x", Var "x") 
let inf_ex_id : string = inference ex_id 
let ex_k : term = Abs ("x", Abs ("y", Var "x")) 
let inf_ex_k : string = inference ex_k
let ex_s : term = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = inference ex_s 
let ex_nat1 : term = App (Abs ("x", Add(Var "x", N 1)), N 3)
let inf_ex_nat1 : string = inference ex_nat1
let ex_nat2 : term = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = inference ex_nat2
let ex_omega : term = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = inference ex_omega
let ex_nat3 : term = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = inference ex_nat3

let main () =
 print_endline "======================";
 print_endline inf_ex_id;
 print_endline "======================";
 print_endline inf_ex_k;
 print_endline "======================";
 print_endline inf_ex_s;
 print_endline "======================";
 print_endline inf_ex_omega;
 print_endline "======================";
 print_endline inf_ex_nat1;
 print_endline "======================";
 print_endline inf_ex_nat2;
 print_endline "======================";
 print_endline inf_ex_nat3

let _ = main ()
