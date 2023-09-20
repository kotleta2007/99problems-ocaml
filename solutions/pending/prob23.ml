Random.init 0;;
(* - : unit = () *)
(* let rec rand_select l n =
let rand_index = (Random.int (List.length l)) in
let rand_elem = List.nth l rand_index in
let remaining = remove_at rand_index l in
if n = 1
then rand_elem
else rand_elem :: (rand_select remaining (n - 1));; *)
(* Error: Unbound value remove_at *)
let rec split_tail l1 l2 n =
        match l2 with
        | [] -> (l1, [])
        | h :: t -> match n with
                | 0 -> (l1, l2)
                | _ -> split_tail (l1 @ [h]) t (n - 1);;
(* val split_tail :
  'a list -> 'a list -> int -> 'a list * 'a list = <fun> *)
let split l n = split_tail [] l n;;
(* val split : 'a list -> int -> 'a list * 'a list = <fun> *)
let remove_at k l =
let (left, right) = split l k in
match right with
| [] -> left
| h :: t -> left @ t;;
(* val remove_at : int -> 'a list -> 'a list = <fun> *)
(* let rec rand_select l n =
let rand_index = (Random.int (List.length l)) in
let rand_elem = List.nth l rand_index in
let remaining = remove_at rand_index l in
if n = 1
then rand_elem
else rand_elem :: (rand_select remaining (n - 1));; *)
(* Error: This expression has type 'a list
       but an expression was expected of type 'a
       The type variable 'a occurs inside 'a list *)
let rec rand_select l n =
let rand_index = (Random.int (List.length l)) in
let rand_elem = List.nth l rand_index in
let remaining = remove_at rand_index l in
if n = 1
then [rand_elem]
else rand_elem :: (rand_select remaining (n - 1));;
(* val rand_select : 'a list -> int -> 'a list = <fun> *)
rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
(* - : string list = ["g"; "d"; "b"] *)
