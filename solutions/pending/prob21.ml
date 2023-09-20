let rec split_tail l1 l2 n =
        match l2 with
        | [] -> (l1, [])
        | h :: t -> match n with
                | 0 -> (l1, l2)
                | _ -> split_tail (l1 @ [h]) t (n - 1);;
(* val split_tail : 'a list -> 'a list -> int -> 'a list * 'a list = <fun> *)
let split l n = split_tail [] l n;;
(* val split : 'a list -> int -> 'a list * 'a list = <fun> *)
(* let insert_at x k l =
let (left, right) = split l k in
match right with
| [] -> left @ [h]
| h :: t -> left @ (h :: t);; *)
(* Error: Unbound value h *)
let insert_at x k l =
let (left, right) = split l k in
match right with
| [] -> left @ [x]
| h :: t -> left @ (x :: t);;
(* val insert_at : 'a -> int -> 'a list -> 'a list = <fun> *)
insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "alfa"; "c"; "d"] *)
let insert_at x k l =
let (left, right) = split l k in
left @ [x] @ right;;
(* val insert_at : 'a -> int -> 'a list -> 'a list = <fun> *)
insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "alfa"; "b"; "c"; "d"] *)
insert_at "alfa" 3 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "b"; "c"; "alfa"; "d"] *)
insert_at "alfa" 4 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "b"; "c"; "d"; "alfa"] *)
