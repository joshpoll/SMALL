open SML;

let ex0 = "5";

let ex1 = "let val x = 5 in x end";

let ex2 = "val x = 34";

let ex3 = "val x = 34;
val y = 17";

let ex4 = "()";

let ex5 = "(5, 78)";

let ex5_5 = "val (x, y) = (5, 78)";

let ex6 = "val z = 5 + 78";

let ex7 = "val x = 34;
val y = 17;
val z = 5 + 78";

let ex8 = "val x = 34;
val y = 17;
val z = (x + y) + (y + 2)";

let ex9 = "val x = 34;
val y = 17;
val z = (x + y) + (y + 2);
val q = z + 1";

let ex10 = "val a = 1;
val b = 2;
val a = 3";

let ex11 = "if true then 1 else 2";

let ex11_5 = "if false then 1 else 2";

let ex12 = "val x = 34;
val y = 17;
val z = (x + y) + (y + 2);
val q = z + 1;
val abs_of_z = if z < 0 then 0 - z else z";

let idEx = "fun id x = x

id 5";

/* This example seems to be too big. Need to switch to lazy execution of some things to boost
   performance. */
let lec02_1 = "fun pow(x,y) =
  if y=0
  then 1
  else x * pow(x,y-1)

fun cube x =
  pow(x,3)

val sixtyfour = cube 4

val fortytwo = pow(2,2+2) + pow(4,2) + cube(2) + 2";

let lec02_1_simpler = "fun pow(x,y) =
  if y=0
  then 1
  else x * pow(x,y-1)

fun cube x =
  pow(x,3)

val sixtyfour = cube 4";

let lec02_2_simple = "fun swap pr =
  (#2 pr, #1 pr)

fun sum_two_pairs (pr1, pr2) =
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun sort_pair pr =
  if (#1 pr) < (#2 pr)
  then pr
  else (#2 pr, #1 pr)";

let lec02_3 = "val x1 = (7, (true, 9))
val x2 = #1 (#2 x1)
val x3 = (#2 x1)
val x4 = ((3, 5), ((4, 8), (0, 0)))";

/* TODO: this appears to cause problems even though the good style version doesn't. Not sure why. */
let lec02_4_sum_list = "fun null nil                = true
  | null _                  = false

fun hd(x::l')               = x
  | hd  _                   = raise Empty

fun tl(x::l')               = l'
  | tl  _                   = raise Empty

fun sum_list xs =
  if null xs
  then 0
  else hd(xs) + sum_list(tl(xs))

val _ = sum_list [1]";

let lec02_4_sum_list_good_style = "fun sum_list [] = 0
  | sum_list (x::l') = x + sum_list l'

val it = sum_list [1, 2, 3]";

let valbind_pattern = "val (x, y) = (1, 2)";

let nestedLets = "let
  val x = 1
in
  (let val x = 2 in x + 1 end) + (let val y = x + 2 in y + 1 end)
end";