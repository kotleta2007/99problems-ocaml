let rec split_tail l1 l2 n =
        match l2 with
        | [] -> (l1, [])
        | h :: t -> match n with
                | 0 -> (l1, l2)
                | _ -> split_tail (l1 @ [h]) t (n - 1);;
let split l n = split_tail [] l n;;

let rec rotate l n =
	let (left, right) = 
		if n >= 0 
		then (split l n)
		else (split l (List.length l + n)) 
	in (right @ left);;

(* TEST CASES:

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;

*)
