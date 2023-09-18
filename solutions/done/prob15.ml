let rec repeat_tail x n l = 
	if n = 0 then l 
	else (repeat_tail x (n-1) (x :: l));;

let repeat x n = repeat_tail x n [];;

let rec replicate l n = 
	match l with
	| [] -> []
	| h :: t -> (repeat h n) @ (replicate t n);;

(* TEST CASES:

replicate ["a"; "b"; "c"] 3;;

*)
