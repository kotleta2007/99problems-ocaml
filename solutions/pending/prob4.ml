let rec length_tail l n =
match l with
| [] -> n
| _ :: t -> length_tail t (n + 1);;
(* val length_tail : 'a list -> int -> int = <fun> *)
let length l = length_tail l 0;;
(* val length : 'a list -> int = <fun> *)
length ["a"; "b"; "c"];;
(* - : int = 3 *)
length [];;
(* - : int = 0 *)
