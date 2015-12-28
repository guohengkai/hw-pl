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
  | head::tail => if same_string(str, head)
                  then SOME tail
                  else case all_except_option(str, tail) of
                         NONE => NONE
                       | SOME res_list => SOME (head::res_list)

fun get_substitutions1(str_lists, str) =
  case str_lists of
    [] => []
  | head::tail => let
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
        | head::tail => case all_except_option(str, head) of
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
      | head::tail => {first = head, middle = middle, last = last}::replace_first_name(tail)         
  in
    full_name::replace_first_name(sub_list)
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
fun card_color(s, r) =
  case s of
    Clubs => Black
  | Spades => Black
  | Diamonds => Red
  | Hearts => Red

fun card_value(s, r) =
  case r of
    Num num => num
  | Ace => 11
  | _ => 10

fun remove_card(cards, card, ex) =
  case cards of
    [] => raise ex
  | head::tail => if card = head
                  then tail
                  else head::remove_card(tail, card, ex)

fun all_same_color(cards) =
  case cards of
    [] => true
  | _::[] => true
  | head::(neck::rest) =>  (card_color(head) = card_color(neck)) andalso all_same_color(neck::rest)

fun sum_cards(cards) =
  let
    fun sum_cards_internal(cards, sum) =
      case cards of
        [] => sum
      | head::tail => sum_cards_internal(tail, sum + card_value(head))
  in
    sum_cards_internal(cards, 0)
  end

fun sum_to_pre_score(sum, goal) =
  if sum > goal
  then 3 * (sum - goal)
  else goal - sum

fun score(cards, goal) =
  let
    val pre_score = sum_to_pre_score(sum_cards(cards), goal)
  in
    if all_same_color(cards)
    then pre_score div 2
    else pre_score
  end

fun officiate(cards, moves, goal) =
  let
    fun officiate_internal(held_cards, cards, moves) =
      case moves of
        [] => score(held_cards, goal)
      | (Discard card)::rest_moves =>
          officiate_internal(remove_card(held_cards, card, IllegalMove), cards, rest_moves)
      | Draw::rest_moves => case cards of
                      [] => score(held_cards, goal)
		    | head::tail =>
                        let
                          val new_held_cards = head::held_cards
                        in
                          if sum_cards(new_held_cards) > goal
                          then score(new_held_cards, goal)
                          else officiate_internal(new_held_cards, tail, rest_moves)
                        end
  in
    officiate_internal([], cards, moves)
  end

fun score_challenge(cards, goal) =
  let
    fun score_internal(held_cards, cards) =
      case cards of
        [] => score(held_cards, goal)
      | head::tail => 
          let
            val score_ori = score_internal(head::held_cards, tail)
          in
            case head of
              (s, Ace) => Int.min(score_ori, score_internal((s, Num 1)::held_cards, tail))
            | _ => score_ori
          end
  in
    score_internal([], cards)
  end



fun officiate_challenge(cards, moves, goal) =
  let
    fun min_sum_cards(cards) =
      let
        fun min_card_value(s, r) =
          case r of
            Ace => 1
          | _ => card_value((s, r))

        fun sum_cards_internal(cards, sum) =
          case cards of
            [] => sum
          | head::tail => sum_cards_internal(tail, sum + min_card_value(head))
      in
        sum_cards_internal(cards, 0)
      end

    fun officiate_internal(held_cards, cards, moves) =
      case moves of
        [] => score_challenge(held_cards, goal)
      | (Discard card)::rest_moves =>
          officiate_internal(remove_card(held_cards, card, IllegalMove), cards, rest_moves)
      | Draw::rest_moves =>
          case cards of
            [] => score_challenge(held_cards, goal)
          | head::tail =>
              let
                val new_held_cards = head::held_cards
              in
                if min_sum_cards(new_held_cards) > goal
                then score_challenge(new_held_cards, goal)
                else officiate_internal(new_held_cards, tail, rest_moves)
              end
  in
    officiate_internal([], cards, moves)
  end

fun careful_player(cards, goal) =
  let
    fun if_discard_then_draw(cards, card) =
      let
        val sum = sum_cards(card::cards)
        fun if_discard_then_draw_internal(cards) =
          case cards of
            [] => NONE
	  | head :: tail => if sum - card_value(head) = goal
                            then SOME head
                            else if_discard_then_draw_internal(tail)
      in
        if_discard_then_draw_internal(cards)
      end

    fun player_internal(held_cards, cards) =
      case cards of
        [] => []
      | head::tail => 
          let val sum = sum_cards(held_cards)
              val new_held_cards = head::held_cards
          in
            if sum < goal - 10
            then Draw::player_internal(new_held_cards, tail)
            else
              case if_discard_then_draw(held_cards, head) of
                SOME card => [Discard card, Draw]
	      | NONE => if sum + card_value(head) <= goal
                        then Draw::player_internal(new_held_cards, tail)
                        else []
          end
  in
    player_internal([], cards)
  end
