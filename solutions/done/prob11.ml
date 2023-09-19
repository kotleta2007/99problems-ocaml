type 'a rle =
| One of 'a
| Many of int * 'a;;

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

let rec encode_packed l =
	match l with
	| [] -> []
	| h :: t -> 
		match (List.length h) with
		| 1 -> One (List.hd h) :: (encode_packed t)
		| _ -> Many (List.length h, List.hd h) :: (encode_packed t);;

let encode l = encode_packed (pack l);;

(* TEST CASES:

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

*)
