#use "../pending/prob10.ml";;
encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)
type 'a rle =
| One of 'a
| Many of int * 'a;;
(* type 'a rle = One of 'a | Many of int * 'a *)
let rec encode_tail l =
match l with
| [] -> []
| h :: t -> 
match (List.length h) with
| 1 -> One (List.hd h) :: (encode_tail t);;
(* val encode_tail : 'a list list -> 'a rle list = <fun> *)
let rec encode_tail l =
match l with
| [] -> []
| h :: t -> 
match (List.length h) with
| 1 -> One (List.hd h) :: (encode_tail t)
| _ -> Many (List.length h, List.hd h) :: (encode_tail t);;
(* val encode_tail : 'a list list -> 'a rle list = <fun> *)
let encode l = encode_tail (pack l);;
(* val encode : 'a list -> 'a rle list = <fun> *)
encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a");
 One "d"; Many (4, "e")] *)
