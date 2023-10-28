let is_prime n = 
	let int_sqrt n = 
		int_of_float 
		(Float.floor 
		(Float.sqrt 
		(float_of_int n))) in
	let rec range a b = 
		if a <= b then a :: range (a + 1) b 
		else [] in
	if n = 1 then false 
	else List.fold_left 
		(fun acc k -> acc && (n mod k != 0)) 
		true 
		(range 2 (int_sqrt n));;

(* TEST CASES:

not (is_prime 1);;
is_prime 7;;
not (is_prime 12);;

*)
