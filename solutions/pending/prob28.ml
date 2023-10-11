(* let rec quick_sort l comp =
| [] -> []
| h :: t ->
let before = List.filter (not comp h) t in
let after  = List.filter (comp h) t in
before @ [h] @ after;; *)
(* Error: Syntax error *)
(* let rec quick_sort l comp =
| [] -> []
| h :: t ->
let before = List.filter (not comp h) t in
let after  = List.filter (comp h) t in
before @ [h] @ after;; *)
(* Error: Syntax error *)
(* let rec quick_sort l comp =
| [] -> []
| h :: t ->
let before = List.filter (not comp h) t in
let after  = List.filter (comp h) t in
before @ [h] @ after;; *)
(* Error: Syntax error *)
(* let rec quick_sort l comp =
| [] -> []
| h :: t ->
let before = List.filter (not comp h) t in
let after  = List.filter (comp h) t in
before @ [h] @ after;; *)
(* Error: Syntax error *)
(* let rec quick_sort l comp =
| [] -> []
| h :: t ->
let before = List.filter (not comp h) t in
let after  = List.filter (comp h) t in
before @ [h] @ after;; *)
(* Error: Syntax error *)
(* let rec quick_sort l comp = 
match l with
| [] -> []
| h :: t ->
let before = List.filter (not comp h) t in
let after  = List.filter (comp h) t in
before @ [h] @ after;; *)
(* Error: This function has type bool -> bool
       It is applied to too many arguments; maybe you forgot a `;'. *)
(* let rec quick_sort l comp = 
match l with
| [] -> []
| h :: t ->
let before = List.filter (not (comp h)) t in
let after  = List.filter (comp h) t in
before @ [h] @ after;; *)
(* Error: This expression has type bool but an expression was expected of type
         'a -> bool *)
let rec quick_sort l comp = 
match l with
| [] -> []
| h :: t ->
let before = List.filter (fun x -> comp x h) t in
let after  = List.filter (fun x -> comp h x) t in
before @ [h] @ after;;
(* val quick_sort : 'a list -> ('a -> 'a -> bool) -> 'a list = <fun> *)
quick_sort [5; 3; 8; 2; 1; 4] (fun x y -> x < y);;
(* - : int list = [3; 2; 1; 4; 5; 8] *)
let rec quick_sort l comp = 
match l with
| [] -> []
| h :: t ->
let before = quick_sort (List.filter (fun x -> comp x h) t) comp in
let after  = quick_sort (List.filter (fun x -> comp h x) t) comp in
before @ [h] @ after;;
(* val quick_sort : 'a list -> ('a -> 'a -> bool) -> 'a list = <fun> *)
quick_sort [5; 3; 8; 2; 1; 4] (fun x y -> x < y);;
(* - : int list = [1; 2; 3; 4; 5; 8] *)
let length_sort l = quick_sort l (fun x y -> List.length x < List.length y);;
(* val length_sort : 'a list list -> 'a list list = <fun> *)
length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
(* - : string list list =
[["o"]; ["d"; "e"]; ["a"; "b"; "c"]; ["i"; "j"; "k"; "l"]] *)
length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
(* - : string list list =
[["o"]; ["d"; "e"]; ["a"; "b"; "c"]; ["i"; "j"; "k"; "l"]] *)
quick_sort [5; 3; 8; 2; 1; 4; 4] (fun x y -> x < y);;
(* - : int list = [1; 2; 3; 4; 5; 8] *)
let rec quick_sort l comp = 
match l with
| [] -> []
| h :: t ->
let before = quick_sort (List.filter (fun x -> comp x h) t) comp in
let after  = quick_sort (List.filter (fun x -> not (comp x h)) t) comp in
before @ [h] @ after;;
(* val quick_sort : 'a list -> ('a -> 'a -> bool) -> 'a list = <fun> *)
quick_sort [5; 3; 8; 2; 1; 4; 4] (fun x y -> x < y);;
(* - : int list = [1; 2; 3; 4; 4; 5; 8] *)
length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
(* - : string list list =
[["o"]; ["d"; "e"]; ["a"; "b"; "c"]; ["i"; "j"; "k"; "l"]] *)
quick_sort [5; 3; 8; 2; 1; 4; 4] (fun x y -> x < y);;
(* - : int list = [1; 2; 3; 4; 4; 5; 8] *)
quick_sort [5; 3; 8; 2; 1; 4; 4; 5] (fun x y -> x < y);;
(* - : int list = [1; 2; 3; 4; 4; 5; 5; 8] *)
quick_sort [5; 3; 8; 2; 1; 4; 4; 8; 5] (fun x y -> x < y);;
(* - : int list = [1; 2; 3; 4; 4; 5; 5; 8; 8] *)
length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
(* - : string list list =
[["o"]; ["d"; "e"]; ["a"; "b"; "c"]; ["i"; "j"; "k"; "l"]] *)
let length_sort l = quick_sort l (fun x y -> (List.length x) < (List.length y));;
(* val length_sort : 'a list list -> 'a list list = <fun> *)
length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
(* - : string list list =
[["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]] *)
(* let rec length_frequency l x =
l match with
| [] -> 0
| h :: t when (List.length x == List.length h) -> 1 + length_frequency t x
| h :: t -> length_frequency t x;; *)
(* Error: Syntax error *)
let rec length_frequency l x =
match l with
| [] -> 0
| h :: t when (List.length x == List.length h) -> 1 + length_frequency t x
| h :: t -> length_frequency t x;;
(* val length_frequency : 'a list list -> 'b list -> int = <fun> *)
(* let frequency_sort l = quick_sort l (fun x y -> (length_frequency l x) < (length_frequency l y);; *)
(* Error: Syntax error: ')' expected, the highlighted '(' might be unmatched *)
let frequency_sort l = quick_sort l (fun x y -> (length_frequency l x) < (length_frequency l y));;
(* val frequency_sort : 'a list list -> 'a list list = <fun> *)
frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
(* - : string list list =
[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]] *)
