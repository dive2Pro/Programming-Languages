fun hd lst =
  case lst of
      []  => raise List.Empty
    | x :: _ => x;

fun maxlist (xs, ex) =
  case xs of
      [] => raise ex
    | x :: [] =>  x
    | x :: xs' => Int.max(x, maxlist(xs', ex));

exception MyUndesirableCondition;
exception MySecond;
val x = maxlist ([], MyUndesirableCondition)
        (* only handle that expresstion in here*)
        handle MyUndesirableCondition => 42;

exception MyException of int ;

fun f n =
  if n = 0
  then raise List.Empty
  else
      if n = 1
      then raise (MyException 4)
      else n * n;

(f 1 handle List.Empty => 42) handle MyException n => f n;
