let rec length_tail l n =
	match l with
	| [] -> n
	| _ :: t -> length_tail t (n + 1);;

let length l = length_tail l 0;;

(* TEST CASES:

length ["a"; "b"; "c"];;
length [];;

*)
