(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put your solutions for problem 1 here *)

fun len lst =
  case lst of
      [] => 0
    | _ :: lst' => 1 + (len lst');
fun remove_repeat (s: string, ss: string list) =
  case ss of [] => []
           | s' :: ss' => if same_string (s, s')
                          then remove_repeat(s, ss')
                          else s' :: remove_repeat(s, ss')

fun all_except_option (s: string, ss: string list) =
  let val result = remove_repeat(s, ss)
  in if (len ss) = (len result)
     then NONE else SOME(result) end;

all_except_option("2", ["1", "2", "3","2", "5"]);

(* (string list) list * string => string list *)
fun get_substitutions1 (lss: (string list) list, s: string) =
(* the result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result *)
  case lss of [] => []
            | ls'::lss' =>
              let val result = remove_repeat(s, ls')
              in if (len ls') = (len result)
                 then get_substitutions1(lss', s)
                 else result @ get_substitutions1(lss', s) end;
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff");

fun get_substitutions2 (lss: (string list) list, s: string) =
  (* the result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result *)
  let fun aux(lss:(string list) list, ls2) =
        case lss of
            [] => ls2
          | ls'::lss' =>
            let val result = remove_repeat(s, ls')
            in if (len ls') = (len result)
               then aux(lss', ls2)
               else aux(lss', ls2 @ result)
            end
  in
      aux(lss, [])
  end;

get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff");


fun similar_names ( sl: (string list) list, fullname: {first: string, middle: string, last: string}) =
  let val {first, middle, last} = fullname
      val substitutions = get_substitutions2(sl, first);
      fun zip (sst: string list) =
        case sst of
            [] => []
          | s':: sst' => ({first = s', last = last, middle = middle}) :: zip(sst');
  in
      fullname :: zip(substitutions)
  end;
similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"});

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

              (* put your solutions for problem 2 here *)
(* card => color *)
fun card_color (c : card ) =
  case c of
      (Clubs, _) => Black
    | (Spades, _) => Black
    | (_, _) => Red;
fun card_value (c: card) =
  case c of
      (_ , Num i) => i
    | (_, Ace) => 11
    | (_, _) => 10;

val c : card = (Spades, Num(2));
card_color(c) = Black;
card_value c = 2;
card_value(Spades, Ace) = 11 ;
card_value(Spades, Jack) = 10;
card_value(Spades, Queen) = 10;

(* card list * card * exception => card list*)
fun remove_card (cs: card list, c : card ,e) =
  case cs of
      (* if c is not in the list , raise e *)
      [] => raise e
    | c':: cs' =>  if ( c' = c )
  (* if c is in the list more than once , remove only the first one, that's mean only remove the c' and return back cs' *)
                    then cs'
                    else c' :: remove_card(cs', c, e);

val cs = [(Clubs, Queen), c];
remove_card(cs, c , IllegalMove) = [(Clubs,Queen)];

(remove_card(cs, (Clubs, Num(2)), IllegalMove)) handle IllegalMove => [];

fun all_same_color (cs: card list) =
  case cs of
      [] => true
    | c' :: cs' =>
      case cs' of
          [] => true
        | c'' :: []=> card_color(c') = card_color(c'')
        | _ :: c''' ::cs'  =>
          card_color(c') = card_color(c''') andalso all_same_color(cs');

all_same_color([(Clubs,Queen),c,(Clubs,Num(2))]) = true;
all_same_color([(Clubs,Queen),c,(Clubs,Num(2)), (Diamonds, Ace)]) = false;

fun sum_cards (cs: card list) =
  let fun aux_sum (cs: card list, acc) =
        case cs of
            [] => acc
          | c::cs' => aux_sum(cs', card_value(c) + acc)
  in
      aux_sum(cs, 0)
  end;
val cs2 = [(Clubs,Queen),(Clubs,Num(2)), (Diamonds, Ace)];
sum_cards(cs2) = 23 ;

fun score (cs : card list, goal: int) =
  (* if sum is greater than goal, the preliminary score is three(sum - goal) *)
  let val sum = sum_cards(cs)
      val preliminary_score = if sum > goal
                              then 3 * ( sum - goal)
                            (* else the preliminary score is (goal - sum)*)
                              else goal - sum;
  (* score is the preliminary score unless all the held-cards are the same color *)
      val is_cards_all_same_color = all_same_color(cs)
  (* in which case the score is the preliminary score divided by 2*)
  in
      if is_cards_all_same_color
      then
          preliminary_score div 2
      else
          preliminary_score
  end;

fun officiate (cs : card list, ms: move list, goal: int ) =
  let
      fun aux_states ( cs: card list , ms: move list , held_cards : card list) =
        case ms of
            [] => score(held_cards, goal)
          | m :: ms' =>
            case m of
                Draw =>
                (* pop cs *)
                (case cs of
                     [] => score(held_cards, goal)
                   | c :: cs' =>
                     let
                         val new_held_cards = c :: held_cards
                         val sum = sum_cards(new_held_cards)
                     in
                                  if sum >= goal
                                  then score(new_held_cards, goal)
                                  else aux_states(cs', ms', new_held_cards)
                     end
                )
              | Discard dc =>
                aux_states(cs, ms', remove_card(held_cards, dc,  IllegalMove))
in
    aux_states(cs, ms,[])
end
