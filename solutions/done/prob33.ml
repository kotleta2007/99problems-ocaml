let rec gcd m n =
	if m = n then m
	else if m > n then gcd (m - n) n
	else gcd (n - m) m;;

let coprime m n =
	gcd m n = 1;;

(* TEST CASES:

coprime 13 27;;
not (coprime 20536 7826);;

*)
