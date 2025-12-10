type 'a list = Nil | Cons of 'a * 'a list

let bool_not b =
  match b with
  | true -> false
  | false -> true
  
let bad_fs =
  Cons ((fun x -> Cons(x, Nil)), Cons (bool_not, Nil))
