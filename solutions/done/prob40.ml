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

let goldbach n =
	if n mod 2 = 1 || n < 4 then (-1, -1)
	else let primes = all_primes 2 n in
	let rec iter l = 
		match l with
		| [] -> (-1, -1)
		| h :: t -> 
			let complement = n - h in
			if (is_prime complement) then (h, complement)
			else iter t in 
	iter primes;; 

(* TEST CASES:

goldbach 28;;

*)
