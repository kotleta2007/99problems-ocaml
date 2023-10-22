let rec rev_tail l res =
	match l with
	| [] -> res
	| h :: t -> rev_tail t (h :: res);;
let rev l = rev_tail l [];;

let rec gray n =
	if n = 1 then ["0"; "1"]
	else 
		let prev = gray (n - 1) in
		let zero = List.map (fun x -> "0" ^ x) prev in
		let one  = List.map (fun x -> "1" ^ x) (rev prev) in
	zero @ one;;

(* TEST CASES:

gray 1;;
gray 2;;
gray 3;;

*)
