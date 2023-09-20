let rec split_tail l1 l2 n =
        match l2 with
        | [] -> (l1, [])
        | h :: t -> match n with
                | 0 -> (l1, l2)
                | _ -> split_tail (l1 @ [h]) t (n - 1);;
(* val split_tail : 'a list -> 'a list -> int -> 'a list * 'a list =
  <fun> *)
let split l n = split_tail [] l n;;
(* val split : 'a list -> int -> 'a list * 'a list = <fun> *)
let remove_at n l =
let (left, right) = split l (n - 1) in
match right with
| [] -> left
| h :: t -> left @ t;;
(* val remove_at : int -> 'a list -> 'a list = <fun> *)
remove_at 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["b"; "c"; "d"] *)
let remove_at n l =
let (left, right) = split l n in
match right with
| [] -> left
| h :: t -> left @ t;;
(* val remove_at : int -> 'a list -> 'a list = <fun> *)
remove_at 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "c"; "d"] *)
let remove_at k l =
let (left, right) = split l k in
match right with
| [] -> left
| h :: t -> left @ t;;
(* val remove_at : int -> 'a list -> 'a list = <fun> *)
remove_at 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "c"; "d"] *)
