let rec range a b = 
	if a = b then [a] 
	else if a < b then a :: range (a + 1) b 
	else a :: range (a - 1) b;;

(* TEST CASES:

range 4 9;;
range 9 4;;

*)
