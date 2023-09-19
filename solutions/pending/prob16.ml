(* let rec drop l n = drop_counter l n 1;; *)
(* Error: Unbound value drop_counter *)
let rec drop_counter l n counter = 
match l with
| [] -> []
| h :: t -> 
if n = counter
then drop_counter t n 1
else h :: (drop_counter t n (counter + 1));;
(* val drop_counter : 'a list -> int -> int -> 'a list =
  <fun> *)
let drop l n = drop_counter l n 1;;
(* val drop : 'a list -> int -> 'a list = <fun> *)
drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)
