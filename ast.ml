type term =
  | Var of string
  | N of int
  | Add of term * term
  | App of term * term
  | Abs of string * term

type typ =
  | Var of string
  | Nat
  | Arr of typ * typ