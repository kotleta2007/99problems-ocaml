let rec extract n l = 
	if n = 1
	then List.map (fun x -> [x]) l
	else 
		match l with
		| h :: t -> (List.map (fun x -> h :: x) (extract (n-1) t)) @ (extract n t) 
		| [] -> [];;

let rec remove_tail l1 l2 x =
	match l1 with
	| [] -> l2
	| h :: t when h = x -> (List.rev l2) @ t
	| h :: t -> remove_tail t (h :: l2) x;;
let remove l x = remove_tail l [] x;;

let rec remove_list l1 l2 =
	match l1 with
	| [] -> l2
	| h :: t -> remove_list t (remove l2 h);;

let extract_complement n l =
	let subsets = extract n l in
		List.map 
		(fun x -> [x; remove_list x l])
		subsets;;

let rec group l groups =
	match groups with
	| [] -> [[]]
	| h :: t -> 
		let first_grouped = extract_complement h l in
		let unflattened = List.map 
			(fun grouping -> 
				match grouping with
				| [] -> []
				| first :: next -> 
					let next_groups = group (List.hd next) t in
					(List.map (fun x -> first :: x) next_groups)
			)
			first_grouped in
		List.flatten unflattened
		;;

(* TEST CASES:

group ["a"; "b"; "c"; "d"] [2; 1];;

*)
