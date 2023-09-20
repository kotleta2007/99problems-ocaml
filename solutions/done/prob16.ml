let rec drop_counter l n counter = 
	match l with
	| [] -> []
	| h :: t -> 
		if n = counter
		then drop_counter t n 1
		else h :: (drop_counter t n (counter + 1));;
let drop l n = drop_counter l n 1;;

(* TEST CASES:

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

*)
