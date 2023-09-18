let rec rev_tail l res =
	match l with
	| [] -> res
	| h :: t -> rev_tail t (h :: res);;

let rev l = rev_tail l [];;

(* TEST CASES:

rev ["a"; "b"; "c"];;

*)
