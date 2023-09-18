let rec last l =
	match l with
	| [] -> None
	| h :: [] -> Some h
	| h :: t -> last t;;

(* TEST CASES:

last [1; 2; 3; 4];;
last ["a"; "b"; "c"; "d"];;
last [];;

*)
