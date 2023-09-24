let rec split_tail l1 l2 n =
	match l2 with
	| [] -> (l1, [])
	| h :: t -> match n with
		| 0 -> (l1, l2)
		| _ -> split_tail (l1 @ [h]) t (n - 1);;
let split l n = split_tail [] l n;;

let insert_at x k l =
let (left, right) = split l k in
left @ [x] @ right;;

(* TEST CASES:

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 3 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 4 ["a"; "b"; "c"; "d"];;

*)
