let rec split_tail l1 l2 n =
        match l2 with
        | [] -> (l1, [])
        | h :: t -> match n with
                | 0 -> (l1, l2)
                | _ -> split_tail (l1 @ [h]) t (n - 1);;
let split l n = split_tail [] l n;;

let slice l i k =
	let (end_removed, _) = split l (k + 1) in
	let (_, begin_removed) = split end_removed i in
	begin_removed;;

(* TEST CASES:

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;

*)
