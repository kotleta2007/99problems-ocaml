type 'a binary_tree = 
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec insert less l x =
	match l with
	| [] -> [x]
	| h :: t when (less h x) -> h :: (insert less t x)
	| h :: t -> x :: l;;

let rec insertions less sorted unsorted =
	match unsorted with
	| [] -> sorted
	| h :: t -> insertions less (insert less sorted h) t;;

let insertion_sort less l =
	match l with
	| [] -> []
	| h :: t -> insertions less [h] t;;

let fs =
	[("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)];;

let huffman frequencies = 
	let comparator = (fun x y -> (fst x) < (fst y)) in
	let trees = 
		List.map
		(fun p -> ((snd p), Node ([(fst p)], Empty, Empty)))
		frequencies
	in
	let sorted = insertion_sort comparator trees in
	let rec iter l = 
		match l with
		| [] -> failwith "Empty frequency table."
		| h :: [] -> h
		| h1 :: h2 :: t -> 
			match h1 with
			| (h1_count, Node (h1_letters, h1_left, h1_right)) ->
			match h2 with
			| (h2_count, Node (h2_letters, h2_left, h2_right)) ->
			let combined = (h1_count + h2_count, Node (h1_letters @ h2_letters, (snd h1), (snd h2))) in
			iter (insert comparator t combined)
	in
	let huffman_tree = snd (iter sorted) in
	let rec encoding tree symbol = 
		match tree with
		| Empty -> failwith "Empty frequency table."
		| Node ([x], Empty, Empty) when x = symbol -> ""
		| Node (letters, left, right) ->
			if (List.mem symbol letters)
			then 
				match left, right with
				| Node (left_letters, _, _), Node (right_letters, _, _) ->
					if (List.mem symbol left_letters) 
					then "0" ^ (encoding left symbol)
					else "1" ^ (encoding right symbol)
			else failwith "Symbol not found."
	in
	let huffman_coding =
		List.map
		(fun x -> (fst x, (encoding huffman_tree (fst x))))
		frequencies
	in
	insertion_sort (fun x y -> (snd x) < (snd y)) huffman_coding;;
	
(* TEST CASES:

huffman fs;;
huffman [("a", 10); ("b", 15); ("c", 30); ("d", 16); ("e", 29)];;

*)
