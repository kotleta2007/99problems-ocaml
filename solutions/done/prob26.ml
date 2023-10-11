let rec extract n l = 
	if n = 1
	then List.map (fun x -> [x]) l
	else match l with
		| h :: t -> (List.map (fun x -> h :: x) (extract (n-1) t)) @ (extract n t) 
		| [] -> [];;

(* TEST CASES:

extract 2 ["a"; "b"; "c"; "d"];;

*)
