(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)

exception NoAnswer

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

(* Calculate the sum of all the pattern. The value of pattern is definited as following:
   -  Wildcard: the return value of f1 with no argument
   -  Variable x: the return value of f2 with x
   -  TupleP: sum of all the pattern in the list
   -  ConstructorP: the value of pattern
*)
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
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str, 0)))

val longest_string1 =
  List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) ""

val longest_string2 =
  List.foldl (fn (x, y) => if String.size(x) < String.size(y) then y else x) ""

fun longest_string_helper f =
  List.foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) ""
val longest_string3 = longest_string_helper Int.>
val longest_string4 = longest_string_helper Int.>=

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f list =
  case list of
    [] => raise NoAnswer
  | head::tail => case f head of
                    SOME res => res
	          | NONE => first_answer f tail

fun all_answers f list =
  let
    fun all_answers_internal(list, res) =
      case list of
        [] => SOME res
      | head::tail => case f(head) of
                        NONE => NONE
		      | SOME lst => all_answers_internal(tail, res @ lst)
  in
    all_answers_internal(list, [])
  end

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var(var, pattern) =
  g (fn () => 0) (fn str => if str = var then 1 else 0) pattern

fun check_pat pattern =
  let
    fun get_variable_list(pattern, lst) =
      case pattern of
        Variable s => s::lst
      | ConstructorP(_, p) => get_variable_list(p, lst)
      | TupleP ps => List.foldl (fn (p, l) => get_variable_list(p, l)) lst ps
      | _ => lst
    
    fun check_replicate lst =
      case lst of
        [] => false
      | head::tail => List.exists (fn str => str = head) tail
                        orelse check_replicate tail
  in
    not (check_replicate (get_variable_list(pattern, [])))
  end

fun match(valu, pattern) =
  case (valu, pattern) of
    (_, Wildcard) => SOME []
  | (_, Variable s) => SOME [(s, valu)]
  | (Unit, UnitP) => SOME []
  | (Const x, ConstP y) => if x = y then SOME [] else NONE
  | (Tuple vs, TupleP ps) => if List.length vs  = List.length ps
                             then all_answers (match) (ListPair.zip(vs, ps))
			     else NONE
  | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match(v, p) else NONE
  | _ => NONE

fun first_match valu ps =
  let
    fun curry f x y = f (x, y)
  in
    SOME (first_answer (curry match valu) ps) handle NoAnswer => NONE
  end

fun typecheck_patterns(cons, patterns) =
  let
    fun get_constructor(name, cons) =
      case cons of
        [] => NONE
      | (s1, s2, t)::tail => if s1 = name
			     then SOME (s2, t)
			     else get_constructor(name, tail)

    exception TypeError;
    fun merge_type(t1, t2) =
      if t1 = t2
      then t1
      else case (t1, t2) of
             (TupleT ts1, TupleT ts2) =>
               if List.length(ts1) = List.length(ts2)
               then TupleT (List.map (merge_type) (ListPair.zip(ts1, ts2)))
               else raise TypeError
           | (_, Anything) => t1
           | (Anything, _) => t2
           | _ => raise TypeError

    fun get_pattern_type(pattern) =
      case pattern of
        Wildcard => Anything
      | Variable _ => Anything
      | UnitP => UnitT
      | ConstP _ => IntT
      | TupleP ps => TupleT (List.map get_pattern_type ps)
      | ConstructorP (s, p) =>
          let fun match_constructor(s1, s2, t) =
                s1 = s andalso merge_type(get_pattern_type p, t) = t
          in
	    case List.find match_constructor cons of
              SOME (_, s, _) => Datatype s
	    | NONE => raise TypeError
          end
  in
    SOME (List.foldl (merge_type) Anything (List.map (get_pattern_type) patterns)) handle TypeError => NONE
  end
