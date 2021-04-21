(* absyn.ml *)

type symbol = Symbol.symbol
  [@@deriving show]

type 'a loc = 'a Location.loc
  [@@deriving show]

type operator =
  | Plus
  | LT
  [@@deriving show]

type exp =
  | IntExp of int
  | OpExp of operator * lexp * lexp
  | FuncCallExp of symbol * lexp list
  | DecExp of symbol * lexp * lexp  
  | CondExp of lexp * lexp * lexp
  | IdExp of symbol
  [@@deriving show]




and fundec = (type_ * symbol) * (type_ * symbol) list * lexp
  [@@deriving show]

and type_ =
  | Int
  | Bool
  [@@deriving show]

and lexp = exp loc
  [@@deriving show]

and lfundec = fundec loc
  [@@deriving show]
  
and lfundecs = (lfundec list) loc
  [@@deriving show]