(*


*)

datatype s1 = Deg of int
            | Buzs of s2
     and s2 = Teg of int
            | Has of s1;

fun d1 xs =
  let
      fun h1 xs =
        case xs of
            [] => true
          | 1::xs' => h2 xs'
          | _ =>  false
      and h2 xs =
          case xs of
              [] => false
            | 2 :: xs' =>  h1 xs'
            | _ => false;
  in
      h1 xs
  end;

