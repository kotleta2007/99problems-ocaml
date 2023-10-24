let rec quick_sort l comp = 
	match l with
	| [] -> []
	| h :: t ->
		let before = quick_sort (List.filter (fun x -> comp x h) t) comp in
		let after  = quick_sort (List.filter (fun x -> not (comp x h)) t) comp in
	before @ [h] @ after;;

let length_sort l = 
	quick_sort l (fun x y -> (List.length x) < (List.length y));;

let rec length_frequency l x =
	match l with
	| [] -> 0
	| h :: t when (List.length x == List.length h) -> 1 + length_frequency t x
	| h :: t -> length_frequency t x;;

let frequency_sort l = 
	quick_sort l (fun x y -> (length_frequency l x) < (length_frequency l y));;

(* TEST CASES:

length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;

*)
