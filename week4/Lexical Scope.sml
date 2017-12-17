fun f g =
  let
      (* in ML there are only Lexical Scope works *)
      val x = 3
  in g 2
  end;
val x  = 4;
fun h y = y + x;

f(h);

fun sql_of_abs1 i = (Math.sqrt o Real.fromInt o abs)i;

sql_of_abs1(~ 9);
infix !>
fun x !> f = f x;
fun sql_of_abs2 i = i !> abs !> Real.fromInt !> Math.sqrt;

sql_of_abs2(~ 9);

fun sorted_nicer x y z = z >= y andalso y >= x;

val sn = sorted_nicer 2 4;
sn(5);
fun fold f acc cs = case cs of
                        [] => acc
                      | c::cs' =>
                        fold f (f(acc, c)) cs';

fold (fn (acc, c) => acc + c)  0  [1,2,3];

(*
structure X = List;
signature X = LIST;*)

(* datatype is more general way to produce a type*)

datatype set =  S of {
             insert : int -> set,
             member : int -> bool,
             size : unit -> int
         };
val empty_set =
    (* here is use closure to maintain the data set*)
    let fun make_set xs =
          let
              fun contains i  = List.exists(fn j => i = j) xs
          in
              S {
                  insert = fn i => if contains i
                                   then make_set xs
                                   else
                                       make_set (i :: xs),
                  member = contains,
                  size = fn () => length xs
              }
          end
    in
        make_set []
    end
fun use_sets () =
  let val S s1 = empty_set
      val S s2 = (#insert s1) 34
      val S s3 = (#insert s2) 34
      val S s4 = #insert s3 19
  in
      if (#member s4) 42
      then 99
      else if (#member s4) 19
      then 1 + (#size s3)()
      else 0
  end;

use_sets();

                         
