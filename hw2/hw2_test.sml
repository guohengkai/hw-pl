(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Tests *)

use "hw2.sml";
val test1a_1 = all_except_option("hehe", ["haha", "oh", "xixi"]) = NONE
val test1a_2 = all_except_option("hehe", ["haha", "hehe", "xixi"]) = SOME ["haha", "xixi"]

val test1b_1 = get_substitutions1([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred")
  = ["Fredrick", "Freddie", "F"];
val test1b_2 = get_substitutions1([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff")
  = ["Jeffrey", "Geoff", "Jeffrey"];
val test1c_1 = get_substitutions2([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred")
  = ["Fredrick", "Freddie", "F"];
val test1c_2 = get_substitutions2([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff")
  = ["Jeffrey", "Geoff", "Jeffrey"];

val test1d_1 = similar_names([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], {first = "Fred", middle = "W", last = "Smith"})
  = [{first = "Fred", last = "Smith", middle = "W"},
     {first = "Fredrick", last = "Smith", middle = "W"},
     {first = "Freddie", last = "Smith", middle = "W"},
     {first = "F", last = "Smith", middle = "W"}];

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

val test2a_1 = card_color((Spades, Num 1)) = Black;
val test2a_2 = card_color((Hearts, Jack)) = Red;
val test2b_1 = card_value((Spades, Num 2)) = 2;
val test2b_2 = card_value((Diamonds, Ace)) = 11;
val test2b_3 = card_value((Clubs, Queen)) = 10;

val test2c_1 = remove_card([(Spades, Num 1), (Hearts, Jack), (Spades, Num 2), (Hearts, Jack), (Clubs, Queen)], (Hearts, Jack), IllegalMove) = [(Spades, Num 1), (Spades, Num 2), (Hearts, Jack), (Clubs, Queen)];
val test2c_2 = remove_card([(Spades, Num 1), (Hearts, Jack), (Spades, Num 2), (Hearts, Jack), (Clubs, Queen)], (Hearts, Queen), IllegalMove) = [] handle IllegalMove => true;

val test2d_1 = all_same_color([(Spades, Num 1), (Clubs, Num 2), (Spades, King)]) = true;
val test2d_2 = all_same_color([(Spades, Num 2), (Clubs, Num 1), (Hearts, Queen)]) = false;

val test2e = sum_cards([(Spades, Num 1), (Hearts, Jack), (Spades, Num 2), (Hearts, Jack), (Clubs, Queen)]) = 33;

val test2f_1 = score([(Spades, Num 1), (Clubs, Num 2), (Spades, King)], 12) = 1;
val test2f_2 = score([(Spades, Num 2), (Clubs, Num 1), (Hearts, Queen)], 14) = 1;

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

val test2g_1 = provided_test1() = 0 handle IllegalMove => true;
val test2g_2 = provided_test2() = 3;

