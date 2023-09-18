let rec duplicate l = 
	match l with
	| [] -> []
	| h :: t -> h :: h :: duplicate t;;

(* TEST CASES:

duplicate ["a"; "b"; "c"; "c"; "d"];;

*)
