type term =
  | Var of string
  | N of int
  | Unit
  | Add of term * term
  | Sub of term * term
  | App of term * term
  | Abs of string * term
  | Nil (* empty list*)
  | Cons of term * term (* constructor for lists *)
  | Hd of term (* head of a list *)
  | Tl of term (* tail of a list *)
  | IfZero of term * term * term (* if-then-else for zero? *)
  | IfEmpty of term * term * term (* if-then-else for empty? *)
  | Let of string * term * term (* let binding *)
  | Fix of string * term (* fixed-point operator *)
  | Deref of term (* dereference a reference *)
  | Ref of term (* create a reference *)
  | Assign of term * term (* assignment to a reference *)
  | Region of string

  

type typ =
  | Var of string
  | Nat
  | UnitType
  | RefType of typ
  | Arr of typ * typ
  | List of typ
  | Forall of string * typ
  | WeakVar of string