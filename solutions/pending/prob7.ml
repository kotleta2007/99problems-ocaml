type 'a node =
| One of 'a
| Many of 'a node list;;
(* type 'a node = One of 'a | Many of 'a node list *)
let rec flatten l = 
match l with
| [] -> []
| h :: t -> 
match h with
| One x -> x :: flatten t
| Many xs -> flatten xs @ flatten t;;
(* val flatten : 'a node list -> 'a list = <fun> *)
flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]];;
(* - : string list = ["a"; "b"; "c"; "d"; "e"] *)
