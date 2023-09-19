type 'a rle =
    | One of 'a
    | Many of int * 'a;;
(* type 'a rle = One of 'a | Many of int * 'a *)
let rec repeat_tail x n l = 
        if n = 0 then l 
        else (repeat_tail x (n-1) (x :: l));;
(* val repeat_tail : 'a -> int -> 'a list -> 'a list = <fun> *)
let repeat x n = repeat_tail x n [];;
(* val repeat : 'a -> int -> 'a list = <fun> *)
let rec decode l =
match l with
| [] -> []
| h :: t -> match h with
| Many (n, x) -> (repeat x n) @ (decode t)
| One x -> x :: (decode t);;
(* val decode : 'a rle list -> 'a list = <fun> *)
decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
(* - : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e";
 "e"; "e"; "e"] *)
