let rec range a b = 
if a = b
then [a]
else if a < b
then a :: range (a + 1) b
else a :: range (a - 1) b;;
(* val range : int -> int -> int list = <fun> *)
range 4 9;;
(* - : int list = [4; 5; 6; 7; 8; 9] *)
range 9 4;;
(* - : int list = [9; 8; 7; 6; 5; 4] *)
