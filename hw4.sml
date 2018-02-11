(* SML comments appear like this *)
(* Britton *)

(* #1 - pow *)
fun pow (a, b) = if b = 0 then 1 else a*pow(a,b-1);

(* #2 - sumTo
Adds 1/x to the sumTo of 1/(x-1) until x reaches less than 1
*)
fun sumTo (x : real) = if x < 1.0 then 0.0 else 1.0/x + sumTo(x-1.0);

(* #3 - repeat
Adds the string s to the string s of the repeat with n -1 while n is greater than 0
 *)
fun repeat (s, n) = if n > 0 then s ^ repeat(s,n-1) else "";

(* #4 - binary
This implementation has leading 0's on even numbers but no leading 0's on odd numbers. Adding a leading 0 so it matches the test code, but implementation works without it
*)
fun binary_t x = if x > 0 then if x mod 2 = 1 then binary_t(x div 2) ^ "1"  else binary_t(x div 2) ^ "0"  else "";
fun binary z = "0" ^ binary_t z;

(* #5 - countNegative
checks if the list is empty, if it is it returns 0 and adds the result of the previous recursive process. If the head of the list is < 0 then add 1 to the countNegative of the tail of the list
*)
fun isEmpty d = if length(d) = 0 then true else false;
fun countNegative x = if isEmpty(x) then 0 else if hd(x) < 0 then 1 + countNegative(tl(x)) else 0 + countNegative(tl(x));

(* #6 - absList *)
fun abs_help (d1,d2) = (abs(d1),abs(d2));
fun absList x = if isEmpty(x) then nil else [abs_help(hd(x))]@absList(tl(x));


(* #7 - split
split the head of x if the list is not empty and then add it to the split of tl(x)
*)
fun split x = if isEmpty(x) then nil else if hd(x) mod 2 = 0 then [(hd(x) div 2,hd(x) div 2)]@split(tl(x)) else [(hd(x) div 2,(hd(x) div 2)+1)]@split(tl(x));


(* #8 - isSorted
lists of length 0 are true. If head of x is > then head of tail of x then not sorted. If list length = 1 then true else check the tail of the list.
*)
fun isSorted (x : int list) = if length(x) = 0 then true else if length(x) = 1 then true else if hd(x) > hd(tl(x)) then false else if length(tl(x)) = 1 then true else isSorted(tl(x));


(* #9 - collapse
adds the head of the list and the head of the tail of the list until the list is of length 1 or 0 then it sets the end of the list to the last remaining element or []
*)
fun collapse x = if length(x) = 0 then [] else if length(x) = 1 then x else [hd(x)+hd(tl(x))]@collapse(tl(tl(x)));


(* #10 - insert
adds the head of x to the insert of the tail of x and n if n < the head of x. When n > head of x it simply adds n to the head of x.
*)
fun insert (n, x) = if length(x) = 0 then [n] else if n > hd(x) then [hd(x)]@insert(n,tl(x)) else [n]@x;
