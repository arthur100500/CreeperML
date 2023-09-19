type expr =
  | EInt of int
  | ESum of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | ESub of expr * expr
[@@deriving show { with_path = false }]

let eint n = EInt n
let esum e1 e2 = ESum (e1, e2)
let emul e1 e2 = EMul (e1, e2)
let ediv e1 e2 = EDiv (e1, e2)
let esub e1 e2 = ESub (e1, e2)
