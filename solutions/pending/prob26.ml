let l = ["a"; "b"; "c"; "d"];;
(* val l : string list = ["a"; "b"; "c"; "d"] *)
(* let rec extract n l = 
if n = 1
then List.map (fun x -> [x]) l;; *)
(* Error: This expression has type 'a list list
       but an expression was expected of type unit
       because it is in the result of a conditional with no else branch *)
let rec extract n l = 
if n = 1
then List.map (fun x -> [x]) l
else match l with
| h :: t -> (List.map (fun x -> h :: x) (extract (n-1) t))  @ (extract n t) 
| [] -> [];;
(* val extract : int -> 'a list -> 'a list list = <fun> *)
extract 2 l;;
(* - : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"];
 ["b"; "d"]; ["c"; "d"]] *)
extract 1 l;;
(* - : string list list = [["a"]; ["b"]; ["c"]; ["d"]] *)
extract 3 l;;
(* - : string list list =
[["a"; "b"; "c"]; ["a"; "b"; "d"]; ["a"; "c"; "d"];
 ["b"; "c"; "d"]] *)
extract 4 l;;
(* - : string list list = [["a"; "b"; "c"; "d"]] *)
extract 5 l;;
(* - : string list list = [] *)
"bloody hell, that's genius";;
(* - : string = "bloody hell, that's genius" *)
