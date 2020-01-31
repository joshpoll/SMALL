fun sum_list [] = 0
  | sum_list (x::l') = x + sum_list l'

val it = sum_list [1, 2, 3]