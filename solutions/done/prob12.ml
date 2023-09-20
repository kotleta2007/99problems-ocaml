type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let rec repeat_tail x n l = 
	if n = 0 
	then l 
	else (repeat_tail x (n - 1) (x :: l));;

let repeat x n = repeat_tail x n [];;

let rec decode l =
	match l with
	| [] -> []
	| h :: t -> match h with
		| Many (n, x) -> (repeat x n) @ (decode t)
		| One x -> x :: (decode t);;

(* TEST CASES:

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;

*)
