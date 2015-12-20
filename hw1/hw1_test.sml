use "hw1.sml";
is_older((1991, 12, 12), (1991, 12, 12));
is_older((1991, 12, 15), (1991, 12, 13));
is_older((1990, 12, 12), (1991, 4, 8));

number_in_month([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], 12);
number_in_months([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10]);
dates_in_month([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], 12);
dates_in_months([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10]);

get_nth(["1", "2", "3", "4"], 3);
date_to_string((1991, 12, 12));

number_before_reaching_sum([1, 2, 3, 4, 5], 2);
number_before_reaching_sum([1, 2, 3, 4, 5], 6);
what_month(31);
what_month(32);
month_range(30, 35);

oldest([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)]);
oldest([]);

remove_duplicates([1, 2, 3, 4, 5, 1, 2, 3, 10]);
number_in_months([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10, 12]);
number_in_months_challenge([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10, 12]);
dates_in_months([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10, 12]);
dates_in_months_challenge([(1991, 12, 12), (1991, 10, 11), (1991, 12, 1)], [12, 10, 12]);

reasonable_date((1991, 12, 12));
reasonable_date((0, 12, 12));
reasonable_date((1991, 13, 12));
reasonable_date((1991, 12, ~1));
reasonable_date((1991, 2, 29));
reasonable_date((2000, 2, 29));
reasonable_date((1900, 2, 29));
