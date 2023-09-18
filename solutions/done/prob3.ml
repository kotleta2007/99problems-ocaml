let rec at n l =
	match l with
	| [] -> None
	| h :: [] -> if n = 1 then (Some h) else None
	| h :: t -> if n = 1 then (Some h) else (at (n-1) t);;

let at_std n l = 
	try Some (List.nth l (n-1)) with
	| Failure _ -> None;;

(* TEST CASES: 

at 3 ["a"; "b"; "c"; "d"; "e"];;
at 3 ["a"];;

at_std 3 ["a"; "b"; "c"; "d"; "e"];;
at_std 3 ["a"];;

*)
