let rec rev_tail l res =
match l with
| [] -> res
| h :: t -> rev_tail t (h :: res);;
(* val rev_tail : 'a list -> 'a list -> 'a list = <fun> *)
let rev l = rev_tail l [];;
(* val rev : 'a list -> 'a list = <fun> *)
rev ["a"; "b"; "c"];;
(* - : string list = ["c"; "b"; "a"] *)
