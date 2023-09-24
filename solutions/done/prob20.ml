let rec split_tail l1 l2 n =
	match l2 with
	| [] -> (l1, [])
	| h :: t -> match n with
		| 0 -> (l1, l2)
		| _ -> split_tail (l1 @ [h]) t (n - 1);;
let split l n = split_tail [] l n;;

let remove_at k l =
	let (left, right) = split l k in
		match right with
		| [] -> left
		| h :: t -> left @ t;;

(* TEST CASES:

remove_at 1 ["a"; "b"; "c"; "d"];;

*)
