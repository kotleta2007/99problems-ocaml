let rec last_two l =
	match l with
	| [] | _ :: [] -> None
	| h1 :: h2 :: [] -> Some l
	| _ :: t -> last_two t;;

(* TEST CASES:

last_two ["a"; "b"; "c"; "d"];;
last_two ["a"];;

*)
