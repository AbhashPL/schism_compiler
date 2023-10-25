type prim1 =
  | Add1
  | Sub1
  | Print  
  | IsNum
  | IsBool
  | IsTuple
  | Input

type prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal


type expr =
  | ELet of (string * expr) list * expr
  | ESeq of expr list
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | EIf of expr * expr * expr
  | EApp of string * expr list
  (* | ELambda of string list * expr *)
  | EBool of bool
  | ENumber of int
  | EId of string
  | ETuple of expr list
  | EGetItem of expr * expr

type type_tag =
  | TBool
  | TNum
  | TPair
  (* | TClosure *)


type decl = 
  | DFun of string * string list * expr

type program =
  | Program of decl list * expr

