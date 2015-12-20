(* test for hw1 *)
use "hw1.sml";

val test1_1 = is_older((1991, 12, 12), (1991, 12, 12)) = false;
val test1_2 = is_older((1991, 12, 15), (1991, 12, 13)) = true;
val test1_3 = is_older((1990, 12, 12), (1991, 4, 8)) = false;

val test2 = number_in_month([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], 12) = 2;
val test3 = number_in_months([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10]) = 3;
val test4 = dates_in_month([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], 12) = [(1991, 12, 12), (1991, 12, 1)];
val test5 = dates_in_months([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10]) = [(1991, 12, 12), (1991, 12, 1), (1991, 10, 11)];

val test6 = get_nth(["1", "2", "3", "4"], 3) = "3";
val test7 = date_to_string((1991, 12, 12)) = "December 12, 1991";

val test8_1 = number_before_reaching_sum([1, 2, 3, 4, 5], 2) = 1;
val test8_2 = number_before_reaching_sum([1, 2, 3, 4, 5], 6) = 2;
val test9_1 = what_month(31) = 1;
val test9_2 = what_month(32) = 2;
val test10 = month_range(30, 35) = [1, 1, 2, 2, 2, 2];

val test11_1 = oldest([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)]) = SOME (1991, 12, 12);
val test11_2 = oldest([]) = NONE;

(* Challenge Problems *)
val test12_1= remove_duplicates([1, 2, 3, 4, 5, 1, 2, 3, 10]) = [4, 5, 1, 2, 3, 10];
val test12_2 = number_in_months_challenge([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10, 12]) = 3;
val test12_3 = dates_in_months_challenge([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10, 12]) = [(1991, 10, 11), (1991, 12, 12), (1991, 12, 1)];

val test13_1 = reasonable_date((1991, 12, 12)) = true;
val test13_2 = reasonable_date((0, 12, 12)) = false;
val test13_3 = reasonable_date((1991, 13, 12)) = false;
val test13_4 = reasonable_date((1991, 12, ~1)) = false;
val test13_5 = reasonable_date((1991, 2, 29)) = false;
val test13_6 = reasonable_date((2000, 2, 29)) = true;
val test13_7 = reasonable_date((1900, 2, 29)) = false;
