(* null is a function , hd , tl *)
fun sum_list(xs: int list) =
  if null xs
  then 0
  else hd xs + (sum_list (tl xs));

fun sum_pair_list(xs: (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs);

fun append(xs: int list, ys: int list) =
  if null xs
  then ys
  else (hd xs):: append((tl xs), ys);


fun firsts(xs: (int * int ) list) =
  if null xs
  then []
  else (#1 (hd xs))::firsts(tl xs);

fun max1( xs: int list) =
  if null xs
  then NONE
  else
      let val t1_ans = max1(tl xs)
      in if isSome t1_ans andalso valOf t1_ans > hd xs
         then t1_ans
         else SOME (hd xs)
      end;

fun max2( xs : int list) =
  if null xs
  then NONE
  else
      let
          fun max_nonempty(xs: int list) =
            if null (tl xs)
            then hd xs
            else
                let val tl_ans = max_nonempty(tl xs)
      in SOME(max_nonempty xs)
      end
