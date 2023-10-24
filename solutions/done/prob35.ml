let rec gcd m n =
	if m = n then m
	else if m > n then gcd (m - n) n
	else gcd (n - m) m;;
let coprime m n =
	gcd m n = 1;;

let rec factors_tail n l k = 
	if n = 1 then l
	else if (coprime n k) then factors_tail n l (k+1)
	else factors_tail (n / k) (k :: l) k;;

let factors n = List.rev (factors_tail n [] 2);;

(* TEST CASES:

factors 315;;

*)
