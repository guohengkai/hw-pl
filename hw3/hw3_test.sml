use "hw3.sml";

val test1 = only_capitals(["hehe", "Haha", "GOOD", "eneN"]) = ["Haha", "GOOD"];

val test2_1 = longest_string1(["hi", "hello", "longeststring", "longestSTRING"]) = "longeststring";
val test2_2 = longest_string1([]) = "";
val test3 = longest_string2(["hi", "hello", "longeststring", "longestSTRING"]) = "longestSTRING";
val test4_1 = longest_string3(["hi", "hello", "longeststring", "longestSTRING"]) = "longeststring";
val test4_2 = longest_string4(["hi", "hello", "longeststring", "longestSTRING"]) = "longestSTRING";

val test5 = longest_capitalized(["hehe", "Haha", "GOOD", "eneN"]) = "Haha";
val test6 = rev_string("abcdefg") = "gfedcba";

val test7_1 = first_answer (fn x => if x > 0 then SOME x else NONE) [~2, 1, 2] = 1;
val test7_2 = first_answer (fn x => if x > 0 then SOME x else NONE) [~2] = 0 handle NoAnswer => true;
val test8_1 = all_answers (fn x => if x > 0 then SOME [x] else NONE) [1, 2] = SOME [1, 2];
val test8_2 = all_answers (fn x => if x > 0 then SOME [x] else NONE) [1, ~2, 2] = NONE;
val test8_3 = all_answers (fn x => SOME [x]) [] = SOME [];

val test9_b = count_wildcards(ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "xixixi"]])) = 3;
val test9_c = count_wild_and_variable_lengths(ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "xixixi"]])) = 11;
val test9_d_1 = count_some_var("hi", ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "hi"]])) = 2;
val test9_d_2 = count_some_var("hello", ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "hi"]])) = 0;

val test10_1 = check_pat(ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "hi"]])) = false;
val test10_2 = check_pat(ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hello", UnitP, TupleP [Wildcard, Variable "hi"]])) = true;

val test11_1 = match(Constructor("hehe", Tuple [Unit, Constructor("haha", Const 1), Const 2, Unit, Tuple [Const 0, Unit]]),
  ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "hi"]])) = SOME [("hi", Const 2), ("hi", Unit)];
val test11_2 = match(Constructor("hehe", Tuple [Unit, Constructor("haha", Const 1), Const 2, Unit, Tuple [Const 0]]),
  ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "hi"]])) = NONE;
val test12_1 = first_match (Constructor("hehe", Tuple [Unit, Constructor("haha", Const 1), Const 2, Unit, Tuple [Const 0, Unit]])) [ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "hi"]]), ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "hi"]])] = SOME [("hi", Const 2), ("hi", Unit)];
val test12_2 = first_match (Constructor("hehe", Tuple [Unit, Constructor("haha", Const 1), Const 2, Unit, Tuple [Const 0]])) [ConstructorP("hehe", TupleP [Wildcard, ConstructorP("haha", Wildcard), Variable "hi", UnitP, TupleP [Wildcard, Variable "hi"]])] = NONE;
