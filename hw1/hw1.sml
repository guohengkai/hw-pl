(* date type: int * int * int (year, month, day) *)
fun is_older(x: int * int * int, y: int * int * int) =
  if #1 x = #1 y
  then if #2 x = #2 y
       then #3 x > #3 y
       else #2 x > #2 y
  else #1 x > #1 y

fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else let
         val number_tl = number_in_month(tl dates, month)
       in
         if #2 (hd dates) = month
         then number_tl + 1
         else number_tl
       end

fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then []
  else let
         val months_tl = dates_in_month(tl dates, month)
       in
         if #2 (hd dates) = month
         then (hd dates)::months_tl
         else months_tl
       end

(* no order keeps *)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strs: string list, n: int) =
  if n = 1
  then hd strs
  else get_nth(tl strs, n - 1)

fun date_to_string(date: int*int*int) =
  let
    val month_strs = ["January", "February", "March", "April",
		      "May", "June", "July", "August",
		      "September", "October", "November", "December"]
  in
    get_nth(month_strs, #2 date) ^ " " ^ Int.toString(#3 date)
        ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(ints: int list, sum: int) =
  let
    fun number_before_reaching_sum_internal(ints: int list, sum: int, cnt: int) =
      if sum <= hd ints
      then cnt - 1
      else number_before_reaching_sum_internal(tl ints, sum - hd ints, cnt + 1)
  in
    number_before_reaching_sum_internal(ints, sum, 1)
  end

(* days: [1, 365] *)
fun what_month(day: int) =
  let
    val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(month_days, day) + 1
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
  if null dates
  then NONE
  else let
         fun oldest_noempty(dates: (int * int * int) list) =
           if null (tl dates)
           then hd dates
           else let
	          val oldest_date = oldest_noempty (tl dates)
                in
                  if is_older(hd dates, oldest_date)
                  then hd dates
                  else oldest_date
                end
       in
         SOME (oldest_noempty dates)
       end

(* Challenge Problems *)
fun remove_duplicates(ints: int list) =
  if null ints
  then []
  else let
         val hd_ints = hd ints
         val rest_ints = remove_duplicates(tl ints)

         fun is_in_list(ints: int list) = if null ints then false else
           if hd ints = hd_ints then true else is_in_list(tl ints) in
           if is_in_list(rest_ints) then rest_ints else
           hd_ints::rest_ints end

fun number_in_months_challenge(dates: (int * int * int) list, ms: int list) =
  let
    val ms_unique = remove_duplicates(ms)
  in
    number_in_months(dates, ms_unique)
  end

fun dates_in_months_challenge(dates: (int * int * int) list, ms: int list) =
  let
    val ms_unique = remove_duplicates(ms)
  in
    dates_in_months(dates, ms_unique)
  end

fun reasonable_date(date: int * int * int) =
  let
    val year = #1 date
    val month = #2 date
    val day = #3 date

    fun reasonable_year() =
      year > 0
    fun reasonable_month() =
      month >= 1 andalso month <= 12
    fun is_year_leap() =
      year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    fun reasonable_day() =
      if day < 1
      then false
      else let
             fun is_year_leap() =
               year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)

             val month_days = if is_year_leap()
                              then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                              else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
             fun reasonable_day_internal(month: int, month_days: int list) =
               if month = 1
               then day <= hd month_days
               else reasonable_day_internal(month - 1, tl month_days)
           in
             reasonable_day_internal(month, month_days)
           end
  in
    reasonable_year() andalso reasonable_month() andalso reasonable_day()
  end
