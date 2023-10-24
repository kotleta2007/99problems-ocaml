let rec gcd m n =
	if m = n then m
	else if m > n then gcd (m - n) n
	else gcd (n - m) m;;

let coprime m n =
	gcd m n = 1;;

let rec range a b = 
	if a = b then [a] 
	else if a < b then a :: range (a + 1) b 
	else a :: range (a - 1) b;;

(* if m prime, phi(m) = m-1. *)

let phi m = 
	if m = 1 then 1
	else List.fold_left 
		(fun acc r -> acc + Bool.to_int (coprime r m)) 
		0 
		(range 1 (m-1));;

(* TEST CASES:

phi 10;;
phi 13;;

*)
