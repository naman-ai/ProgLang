fun is_older (date1 : int*int*int, date2 : int*int*int)=
    if (#1 date1 < #1 date2)
    then true
    else if (#1 date1 > #1 date2)
    then false
    else if (#2 date1 < #2 date2)
    then true
    else if (#2 date1 > #2 date2)
    then false
    else if (#3 date1 < #3 date2)
    then true
    else false

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else 0 + number_in_month(tl dates, month)
			     
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
							     
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
			     
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strings : string list, pos : int) =
    if pos <= 1
    then hd strings
    else get_nth(tl strings, pos-1)
		
fun date_to_string (date : int * int * int) =
    get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum (sum : int, nums : int list) =
    if sum <= 0
    then ~1
    else number_before_reaching_sum(sum - hd nums, tl nums) + 1
				       
fun what_month (day : int) =
    1 + number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])

fun month_range (day1 : int, day2 : int) =
    if day1 <= day2
    then what_month(day1) :: month_range(day1 + 1, day2)
    else []
	     
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let val tl_old = oldest(tl dates)
	in
	    if isSome tl_old andalso is_older(valOf tl_old, hd dates)
	    then tl_old
	    else SOME (hd dates)
	end

fun deduplicate (nums : int list) =
    let
	fun exists (xs : int list, num : int) =
		  if null xs
		  then false
		  else hd xs = num orelse exists(tl xs, num)
	fun go_through_list (xs : int list) =
	    if null xs
	    then []
	    else if exists(tl xs, hd xs)
	    then go_through_list(tl xs)
	    else hd xs :: go_through_list(tl xs)
    in
	go_through_list(nums)
    end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    number_in_months(dates, deduplicate(months))
    
fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, deduplicate(months))
