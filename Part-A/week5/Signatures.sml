signature MATHLIB =
sig
    val fact : int -> int
end;

structure MyMathLib :> MATHLIB =
struct
val fact = fn x => 1 + x;
val doubler = fn x => x + x;

end;
