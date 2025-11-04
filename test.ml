
(*Terms*)
type term = 
  Var of string (* Variable *)
  | N of int (* Nombre *)
  | Add of term * term (* Addition *)
  | Sub of term * term (* Soustraction *)
  | Mul of term * term (* Multiplication *)
  | Div of term * term (* Division *)
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
    | N n -> string_of_int N
    | Add (t1, t2) -> '(' ^ (print_term t1) ^ " + " ^ (print_term t2) ^ ')'
    | Sub (t1, t2) -> '(' ^ (print_term t1) ^ " - " ^ (print_term t2) ^ ')'
    | Mul (t1, t2) -> '(' ^ (print_term t1) ^ " * " ^ (print_term t2) ^ ')'
    | Div (t1, t2) -> '(' ^ (print_term t1) ^ " / " ^ (print_term t2) ^ ')'
    | App (t1, t2) -> '(' ^ (print_term t1) ^ " " ^ (print_term t2) ^ ')'
    | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"


(*pretty printer of types*)
let rec print_type (t : typ) : string =
  match t with
    Var x -> x
  | Arr (t1, t2) -> '(' ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ')'
  | Nat -> "Nat"

(* generator of names of variables of types *)
let counter_var : int ref = ref 0

let new_var () : string = 
  compteur_var := !compteur_var +1;
  "T" ^(string_of_int !compteur_var)

exception VarPasTrouve

(* looks for the type of a variable in an environment *)
let rec search_type (v:string) (e:env) : typ =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::q -> if v1 = v then t1 else search_type v q

(* checker of occurrence of variables *)
let rec belongs_type (v:string) (t:typ) : bool = 
  match t with
    Var v1 -> when v1 = v -> true
    | Arr (t1, t2) -> (belongs_type v t1) || (belongs_type v t2)
    | _ -> False

(* replaces a variable by a type in type *)
let rec substitue_type(t:typ) (v:string) (new_typ:typ) : typ =
  match t with
    Var v1 when v1 = v1 -> new_typ
    | Var v2 -> Var v2
    | Arr (t1, t2) -> Arr (substitue_type t1 v new_typ, substitue_type t2 v new_typ)
    | Nat -> Nat

(* replaces a variable by a type in a list of equations *)
let substitue_type_everywhere (e:equations) (v:string) (new_type: typ) : equations = 
  List.map (fun (x, y) -> (substitue_type x v new_type , substitue_type y v new_type)) e


(* generates typing equations from a term *)

(* understand App/ABS more // IMPORTANT*)
let rec generate_equations (te:term) (t:typ) (e:env) : equations = 
  match te with
    Var x -> let tv : typ = search_type x e in [(t,tv)]
    | N _ -> [(t, Nat)]
    | Add (t1, t2) ->
        let eq1 = generate_equations t1 Nae e in 
        let eq2 = generate_equations t2 Nat e in
        (t, Nat) :: (eq1 @ eq2)
    | Sub (t1, t2) ->
        let eq1 = generate_equations t1 Nat e in
        let eq2 = generate_equations t2 Nat e in
        (t, Nat) :: (eq1 @ eq2)
    | Mul (t1, t2) ->
        let eq1 = generate_equations t1 Nat e in
        let eq2 = generate_equations t2 Nat e in
        (t, Nat) :: (eq1 @ eq2)
    | Div (t1, t2) ->
        let eq1 = generate_equations t1 Nat e in
        let eq2 = generate_equations t2 Nat e in
        (t, Nat) :: (eq1 @ eq2)
    | App (t1, t2) ->
        let new_var : string = new_var () in
        let eq1 = generate_equations t1 (Arr (Var new_var, t)) e in
        let eq2 = generate_equations t2 (Var new_var) e in
        eq1 @ eq2
    | Abs (x, t_body) ->
        let new_var1 : string = new_var () 
        and new_var2 : string = new_var () in
        (t, Arr (Var new_var1, Var new_var2)) ::
        (generate_equations t_body (Var new_var2) ((x, Var new_var1)::e))       


exception Unif_fail of string

(* equations zipper *)
type equa_zip = equations * equations

(* rewinds the zipper *)
(* used to recheck if there is conflict*)
let rec rewind (e: equa_zip) =
  match e with 
    ([], _) -> e
    | (c::e1, e2) -> (e1, c::e2)

(* replaces a variable by a type in an equations zipper *)
let substitue_type_zip (e: equa_zip) (v:string) (new_type: typ) : equa_zip =
  match e with
    (eq1, eq2) -> (substitue_type_everywhere eq1 v new_type, substitue_type_everywhere eq2 v new_type)


(* find a type associated to variable in a zipper equation*)
let rec find_type (e: equa_zip) (goal:string) : typ =
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (Var v, t)::q) when v =  goal -> t
  | (_, (t, Var v)::q) when v = goal -> t
  | (e1, c::e2) -> find_type (e1, e2) goal
(* generates a fresh variable name for types *)
let rec unification(e: equa_zip) (goal: string) : typ =
  match e with
    (_, []) -> try find_type e goal with VarPasTrouve -> raise Unif_fail ("Variable " ^ goal ^ " not found")
    (* equation with goal , we skip*)
    | (e1, (Var v1, t2)::e2) when v1 = goal ->  unification ((Var v1, t2)::e1, e2) goal
      (* two variables : replace one with the other *)
    | (e1, (Var v1, Var v2)::e2) ->  unification (substitue_type_zip (rembobine (e1,e2)) v2 (Var v1)) goal
      (* one variable to the left : verify occurence then replace*)
    | (e1, (Var v1, t2)::e2) ->  if appartient_type v1 t2 then raise (Unif_fail ("occurence de "^ v1 ^" dans "^(print_type t2))) else  unification (substitue_type_zip (rembobine (e1,e2)) v1 t2) goal
      (* one variable to the right : verify occurence then replace*)
    | (e1, (t1, Var v2)::e2) ->  if appartient_type v2 t1 then raise (Unif_fail ("occurence de "^ v2 ^" dans " ^(print_type t1))) else  unification (substitue_type_zip (rembobine (e1,e2)) v2 t1) goal
      (* type arrow to both sides : we decompose*)
    | (e1, (Arr (t1,t2), Arr (t3, t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) goal
      (* types fleche à gauche pas à droite : echec  *)
    | (e1, (Arr (_,_), t3)::e2) -> raise (Unif_fail ("type fleche non-unifiable avec "^(print_type t3)))     
      (* types fleche à droite pas à gauche : echec  *)
    | (e1, (t3, Arr (_,_))::e2) -> raise (Unif_fail ("type fleche non-unifiable avec "^(print_type t3)))     
      (* types nat des deux cotes : on passe *)
    | (e1, (Nat, Nat)::e2) -> unification (e1, e2) goal
      (* types nat à gauche pas à droite : échec *)
    | (e1, (Nat, t3)::e2) -> raise (Unif_fail ("type entier non-unifiable avec "^(print_type t3)))     
      (* types à droite pas à gauche : échec *)
    | (e1, (t3, Nat)::e2) -> raise (Unif_fail ("type entier non-unifiable avec "^(print_type t3)))


(* chain generation of equation and unification *)
let inference (t: term) : string =
  let e : equa_zip = ([], generate_equations t (Var "goal") []) in
  try (let res = unification e "goal" in
        (print_term t)^" ***TYPABLE*** avec le type "^(print_type res))
  with Unif_fail msg -> (print_term t)^" ***NOT TYPABLE*** : "^msg


  (* ***EXEMPLES*** *)  
let ex_id : pterm = Abs ("x", Var "x") 
let inf_ex_id : string = inference ex_id 
let ex_k : pterm = Abs ("x", Abs ("y", Var "x")) 
let inf_ex_k : string = inference ex_k
let ex_s : pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = inference ex_s 
let ex_nat1 : pterm = App (Abs ("x", Add(Var "x", N 1)), N 3)
let inf_ex_nat1 : string = inference ex_nat1
let ex_nat2 : pterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = inference ex_nat2
let ex_omega : pterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = inference ex_omega
let ex_nat3 : pterm = App (ex_nat2, ex_id)
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