let rec compress l =
match l with
| [] -> []
| h :: [] -> h :: []
| h1 :: h2 :: t -> if h1 = h2 then compress (h2 :: t) else h1 :: compress (h2 :: t);;
(* val compress : 'a list -> 'a list = <fun> *)
compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)
