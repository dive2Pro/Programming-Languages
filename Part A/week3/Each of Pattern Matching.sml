fun full_name (r) =
  let val (x, y,z) = r
  in
      x ^ "-" ^ y ^ "-" ^ z
  end;

full_name ("2","3","4");

(*full_name("first":"2","middle":"3","last":"4")*)

fun full_name_2 ({x: string,y:string,z:string}) =
  x ^ "-" ^ y ^ "-" ^ z;
full_name_2 ("2","3","3");
