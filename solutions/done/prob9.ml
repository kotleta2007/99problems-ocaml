let rec rev_tail l res =
	match l with
	| [] -> res
	| h :: t -> rev_tail t (h :: res);;
let rev l = rev_tail l [];;

let rec pack_tail l1 l2 =
	match l1 with
	| [] -> l2
	| h1 :: t1 -> match l2 with
	| [] -> pack_tail t1 ([h1] :: [])
	| h2 :: t2 -> 
		if h1 = (List.hd h2) 
		then (pack_tail t1 ([h1 :: h2] @ t2)) 
		else (pack_tail t1 ([h1] :: h2 :: t2));;
let pack l = rev (pack_tail l []);;

(* TEST CASES:

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

*)
