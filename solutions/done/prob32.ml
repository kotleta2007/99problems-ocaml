let rec gcd m n =
	if m = n then m
	else if m > n then gcd (m - n) n
	else gcd (n - m) m;;

(* TEST CASES;

gcd 13 27;;
gcd 20536 7826;;

*)
