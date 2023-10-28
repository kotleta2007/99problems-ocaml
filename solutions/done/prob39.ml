let rec range a b = 
	if a <= b then a :: range (a + 1) b 
	else [];;
let is_prime n = 
	let int_sqrt n = 
		int_of_float 
		(Float.floor 
		(Float.sqrt 
		(float_of_int n))) in
	if n = 1 then false 
	else List.fold_left 
		(fun acc k -> acc && (n mod k != 0)) 
		true 
		(range 2 (int_sqrt n));;

let all_primes a b =
	let l = range a b in
	List.filter is_prime l;;

(* TEST CASES:

List.length (all_primes 2 7920);;

*)
