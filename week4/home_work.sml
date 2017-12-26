(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string

                               (**** you can put all your code here ****)
infix !>;
fun  x !> f = f x;
(* string list -> string list *)

fun only_capitals (ss: string list) =
  List.filter (fn s => String.sub(s ,0) !> Char.isUpper) ss;
(* string list -> string *)
fun longest_string1 (ss: string list) =
  foldl (fn (longest, str) =>
           if  (String.size longest ) < (String.size str )
           then str
           else longest)
        "" ss ;
longest_string1 ["qwe", "w", "__@@qwe"];
longest_string1 [];

fun longest_string2 (ss: string list) =
  foldl (fn (longest, str) =>
            if  (String.size longest ) <= (String.size str )
            then (print str; str)
            else longest)
        "" ss ;
longest_string2 ["qwe", "__@@qwe", "w", "__@@qwe"];

print ("---------------- 4 --------------");
fun longest_string_helper f ss =
  foldl (
      fn (str, longest) =>
         f (longest, str))
        "" ss ;

val longest_string3 = longest_string_helper (fn (longest, str) =>
                                                if (String.size longest) >= (String.size str)
                                                then longest else str);

longest_string3 ["qwe", "__@@qwe", "w", "__@@qwe"];

longest_string3 ["A","B","C"] ;

val longest_string4 = longest_string_helper (
        fn (longest, str) =>
           if (String.size longest) <= (String.size str)
           then (print "=======";print longest; print  str; print "__@@_"; str )
           else (print longest; print  str; print "___"; longest)
    );


longest_string4 ["A","B","C"] ;

val longest_capitalized  =
    (longest_string3 o only_capitals);

longest_capitalized["Awe", "W__@@qwe", "w", "Eqwe__@@qwe"] ;

val rev_string =
  (String.implode o rev o String.explode);

print "------------ 7 ---------";


(* ('a -> 'b option) -> 'a list -> 'b *)
fun first_answer f ls =
  case ls of
      [] => raise NoAnswer
    | i :: ls' =>
      let
          val result = (f (i))
      in
          case result of
              NONE => first_answer f (ls')
            | SOME v => v
      end;




fun all_answers f ls =
  case ls of
      [] => SOME []
    | i :: ls' =>
      let
          val result = f i ;
      in
          case result of
              NONE => NONE
            | SOME v => SOME ([v] @ (valOf o all_answers f ) ls')
      end

datatype pattern = Wildcard
		             | Variable of string
		             | UnitP
		             | ConstP of int
		             | TupleP of pattern list
		             | ConstructorP of string * pattern

datatype valu = Const of int
	            | Unit
	            | Tuple of valu list
	            | Constructor of string * valu

fun g f1 f2 p =
  let
	    val r = g f1 f2
  in
	    case p of
	        Wildcard          => f1 ()
	      | Variable x        => f2 x
	      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	      | ConstructorP(_,p) => r p
	      | _                 => 0
  end;
(*

*)

(*
  I don't believe this was correct!
  either p matches v or not
  (pattern * valu) -> (string * valu ) list
*)
fun eval pv =
  case pv of
      (Wildcard, _) => []
    (* Variable s matches any value v and produces the one-element list holding(s, v)*)
    | (Variable s, v) => [(s,v)]
    (* UnitP matches only Unit and produces the empty list of bindings *)
    | (UnitP, Unit) => []
    (* ConstP 17 matches only Const 17 -> the empty list of bindings
       similarly for other integers
     *)
    | (ConstP 17, Const 17) => []
    (* TupleP ps matches a value of the form Tuple vs if ps and vs have the same length and for all i, the i^th element of ps matches the i^th element of vs. The list of bindings produced is all the lists from the nested pattern matches appended together *)
    | (TupleP (tp:: tps'), Tuple (tv::tvs')) =>
      eval(tp, tv) @ eval(TupleP(tps'), Tuple(tvs'))
    | (ConstructorP (s1, p), (Constructor(s2,v))) =>
      if (s1 = s2) then eval(p,v) else []
    | _ => [];

(*
 (p => int) takes a pattern and returns how many Wildcard pattern it contains

*)
fun count_wildcards p =
  let
      fun update_acc () =
        1
      fun f2  (x) =
        0
  in
      g update_acc f2 p
  end;

fun count_wild_and_variable_lengths p =
  let
      fun count_wildcards () =
        1
      fun count_string_lengths_of_variables v =
        String.size(v)
  in
      g count_wildcards count_string_lengths_of_variables p
  end;

fun count_some_var (s,p) =
  let
      fun f1 () = 0
      fun f2 x =
        if s = x
        then 1
        else 0
  in
      g f1 f2 p
  end;
(* pattern -> bool*)
fun check_pat p =
  let
      fun all_variables_strings p =
        case p of
            Variable s => [s]
          | ConstructorP (_, p') => all_variables_strings p'
          | TupleP (p':: ps) =>
                (all_variables_strings  p' ) @ all_variables_strings(TupleP ps)
          | _ =>  []
      fun has_repeats( ss: string list) =
        case ss of
            [] => false
          | s :: ss' =>
            List.exists (fn s' => s' = s ) ss'
            orelse has_repeats ss'
  in
     (not o has_repeats o all_variables_strings ) p
  end;

(* valu * pattern -> (string * valu) list option *)
(* note that if the value matches but the pattern has no patterns of the form Variable s, then the result is SOME [] *)
(* to helo you think about the pattern matching problems , here are some patterns in "normal SML speak" and their equivalents in the assignment's setting.
 (_, 5) is like
 TupleP[Wildcard,ConsP 5],
 SOME(x, 3) is like
 ConstructorP("SOME", TupleP[Variable "x", ConstP 3]), and
(s, (t, _)) is like
 TupleP[Variable "s", TupleP[Variable "t", Wildcard]] *)
fun match (v,p) =
  case (v,p) of
      (_, Wildcard) => SOME []
    | (_, Variable s) => SOME [(s,v)]
    | (Unit, UnitP) => SOME []
    | (Const ci, ConstP cp) =>  if ci = cp then SOME [] else NONE
    | (Tuple t, TupleP tp) => (all_answers match) (ListPair.zip (t, tp))
    | _ => NONE
;
