let rec split_tail l1 l2 n =
	match l2 with
	| [] -> (l1, [])
	| h :: t -> match n with
		| 0 -> (l1, l2)
		| _ -> split_tail (l1 @ [h]) t (n - 1);;
let split l n = split_tail [] l n;;

(* TEST CASES:

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
split ["a"; "b"; "c"; "d"] 5;;

*)
