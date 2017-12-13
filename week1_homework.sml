fun is_older(d1: int * int * int, d2: int * int *int ) =
  let
      val y1 = #1 d1;
      val y2 = #1 d2;
      val m1 = #2 d1;
      val m2 = #2 d2;
      val day1 = #3 d1;
      val day2 = #3 d2;
  in
      y1 < y2 orelse ( y1 = y2 andalso m1 < m2)
              orelse ( y1 = y2 andalso m1 = m2 andalso day1 < day2)
  end;

fun number_in_month (xd : (int*int *int) list, month: int) =
  if null xd
  then 0
  else
      (if (#2 (hd xd)) = month then 1 else 0 ) + number_in_month(tl xd, month)

fun number_in_months(xd: (int* int * int) list, xm : int list) =
  if null xm then 0
  else number_in_month(xd, hd xm) + number_in_months(xd,tl xm);
fun dates_in_month(xd: (int * int * int ) list, month: int) =
  if null xd then []
  else
      let val tl_dates  = dates_in_month(tl xd, month)
      in if #2 (hd xd) = month
          then (hd xd)::tl_dates
          else tl_dates
      end;
fun dates_in_months(xd: (int * int * int ) list, months: int list) =
  if null months then []
  else dates_in_month(xd, hd months)@dates_in_months(xd, tl months);
fun get_nth(xs: string list, n: int ) =
  if n = 1 then hd xs
  else if null xs then ""
  else get_nth(tl xs, n -1 );
val months_names = ["January","February","March","April","May","June","July","August","September","October","November","December"];
fun date_to_string( date: int*int*int) =
  get_nth(months_names, (#2 date))^" "^Int.toString(#3 date)^", " ^ Int.toString(#1 date)
fun number_before_reaching_sum(sum: int, xi: int list) =
  let fun recusive_add_head(xi: int list,inner_sum: int, ind: int) =
        if inner_sum + hd xi >= sum
        then ind
        else if null (tl xi) then ind + 1
        else recusive_add_head(tl xi, inner_sum + hd xi, ind + 1);
  in
      recusive_add_head(xi,0,0)
  end;
val month_days = [31,28,31,30,31,30,31,31,30,31,30,31];
fun what_month(day: int)=
  number_before_reaching_sum(day, month_days) + 1;
fun month_range(day1: int, day2 : int) =
  if day1 > day2 then [] else
  let fun month_append(m: int) =
        if(m > day2) then []
        else [number_before_reaching_sum(m , month_days) + 1]@month_append(m + 1)
  in month_append(day1)
  end;

fun oldest(xd: (int*int*int) list) =
  if null xd then NONE
  else let val tl_oldest = oldest(tl xd);
      in
          if not(isSome(tl_oldest)) orelse is_older(hd xd, valOf tl_oldest)
          then SOME(hd xd) else tl_oldest
      end;

fun mem(xm: int list, i : int) =
  not(null xm) andalso( i = hd xm orelse mem(tl xm, i))

fun remove_dumplicates(xm: int list) =
    if null xm
    then []
    else
        let val tl_ans = remove_dumplicates(tl xm)
        in
            if mem(tl_ans, hd xm)
            then tl_ans
            else hd xm :: tl_ans
        end
(* core key is make the month in xm is unique*)
fun number_in_months_challenge(xd: (int*int*int) list, xm: int list) =
  number_in_months(xd, remove_dumplicates(xm))
fun dates_in_months_challenge(xd: (int*int*int) list, months: int list) =
  dates_in_months(xd, remove_dumplicates(months));

fun reasonable_date(date: int * int * int) =
  let
      val year = #1 date
      val month = #2 date
      val days = #3 date
      val is_leap_year = (year mod 400) = 0 orelse (year mod 4 = 0 andalso not(year mod 400 =0))
      val feb_days = if is_leap_year then 29 else 28;
      val lengths = [31, feb_days,31,30,31,30,31,31,30,31,30,31]
      fun get_nth(xs: int list, n) =
        if n = 1 then hd xs
        else get_nth(tl xs, n -1 );

  in
      not(year <= 0) andalso not(month < 1 orelse month > 12) andalso not(days < 1) andalso days <= get_nth(lengths, month -1 )
  end
