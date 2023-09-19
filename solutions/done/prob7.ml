type 'a node =
| One of 'a
| Many of 'a node list;;

let rec flatten l = 
	match l with
	| [] -> []
	| h :: t -> 
		match h with
		| One x -> x :: flatten t
		| Many xs -> flatten xs @ flatten t;;

(* TEST CASES:

flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]];;

*)
