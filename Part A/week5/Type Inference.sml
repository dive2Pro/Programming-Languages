val r = ref NONE;
val _ = r:= SOME "hi"

(*

val pairWithOne = List.map( fn x => (x, 1));
*)
fun pairWithOne_correct  xs = List.map(fn x => (x, 1) ) xs


