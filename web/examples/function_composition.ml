let compose f g x = f (g x)

let bool_not b =
  match b with
  | true -> false
  | false -> true

let int_id (x : int) = x

let bad = compose bool_not int_id