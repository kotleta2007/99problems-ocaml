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
split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) *)
split ["a"; "b"; "c"; "d"] 5;;
(* - : string list * string list =
(["a"; "b"; "c"; "d"], []) *)
