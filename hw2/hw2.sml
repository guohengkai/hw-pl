(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, str_list) =
  case str_list of
    [] => NONE
  | head :: tail => if same_string(str, head)
                    then SOME tail
                    else case all_except_option(str, tail) of
                           NONE => NONE
			 | SOME res_list => SOME (head::res_list)

fun get_substitutions1(str_lists, str) =
  case str_lists of
    [] => []
  | head :: tail => let
                      val sub_list = get_substitutions1(tail, str)
                    in
                      case all_except_option(str, head) of
                        NONE => sub_list
		      | SOME res => res @ sub_list
                    end

fun get_substitutions2(str_lists, str) = 
  let fun get_sub_tail_recur(str_lists, res_list) =
        case str_lists of
          [] => res_list
        | head :: tail => case all_except_option(str, head) of
                            NONE => get_sub_tail_recur(tail, res_list)
		          | SOME res => get_sub_tail_recur(tail, res_list @ res)
  in
    get_sub_tail_recur(str_lists, [])
  end

fun similar_names(str_lists, full_name) =
  let
    val {first = first, middle = middle, last = last} = full_name
    val sub_list = get_substitutions2(str_lists, first)
    fun replace_first_name(sub_list) =
      case sub_list of
        [] => []
      | head :: tail => {first = head, middle = middle, last = last} :: replace_first_name(tail)         
  in
    full_name :: replace_first_name(sub_list)
  end
	             
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
