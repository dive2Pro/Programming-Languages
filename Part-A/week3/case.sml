datatype mytype = TwoInts of int * int
                | Str of string
       | Pizza;

fun f x =
  case x of
      Pizza =>3
    | Str s =>  8
    | TwoInts(i1,i2) => i1 +i2;

f(Str "2");

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp;

fun eval e =
  case e of
      Constant i => i
    | Negate e2 => ~ (eval e2)
    | Add(e1,e2) => (eval e1) + (eval e2)
    | Multiply(e1, e2) =>  (eval e1) * (eval e2);

fun number_of_adds e =
  case e of
      Constant i => 0
    | Negate e2 =>  number_of_adds e2
    | Add(e1, e2) => 1 + number_of_adds e1 + number_of_adds  e2
    | Multiply(e1, e2) => number_of_adds e1 + number_of_adds e2;

val add_constant = Add(Constant (10 + 9), Negate( Constant 4));
(* 2 *)
val exp_addcount = number_of_adds(Multiply(add_constant, add_constant));

(* Pattern Matching so far *)

(* Type Synonyms *)

datatype formula = Desk
       | Television
       | IceFrog
       | Lighting;
datatype securate = Police
       | Sky
       | Sea
       | Land;
type thing =  securate * formula

fun is_sky_icefrog (e : thing)=
  (*#1 e = Sky andalso #2 e = IceFrog;*)
  case e of
      (Sky, IceFrog) => true
    | _ => false;

datatype my_int_list = Empty
                     | Cons of int * my_int_list;

val x = Cons(4, Cons(23, Cons(2009,Empty)));

fun append_my_list (xs, ys) =
  case xs of
      Empty => ys
    | Cons(x, xs') => Cons(x, append_my_list(xs',ys));

append_my_list(Cons(4, Empty), Empty);

fun sum_list xs =
  case xs of
      [] => 0
    (* :: like hd *)
    | x::y => x + sum_list y;
sum_list([1,2,3]);

fun append (xs, ys) =
  case xs of
      [] => ys
    | (x :: xs') => x :: append(xs', ys);

append([1,2,3],[4,6]);

