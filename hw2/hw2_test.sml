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
(*
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
*)
