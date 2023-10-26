type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let rec encode_buf l n x =
	match l with
	| [] -> [(n, x)]
	| h :: t -> 
		if h = x
		then encode_buf t (n + 1) x
		else (n, x) :: (encode_buf t 1 h);;

let encode_as_pairs l =
	match l with
	| [] -> []
	| h :: t -> encode_buf t 1 h;;

let encode l = 
	List.map (fun x -> 
		match x with
		| (1, elem) -> One elem
		| (n, elem) -> Many (n, elem)) 
	(encode_as_pairs l);;

(* TEST CASES:

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

*)
