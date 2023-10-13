let rec gcd m n =
if m = n then m
else if m > n then gcd (m - n) n
else gcd (n - m) m;;
(* val gcd : int -> int -> int = <fun> *)
let coprime m n =
gcd m n = 1;;
(* val coprime : int -> int -> bool = <fun> *)
let rec factors_tail n l k = 
if n = 1 then l
else if (coprime n k) then factors_tail n l (k+1)
else factors_tail (n / k) (k :: l) k;;
(* val factors_tail : int -> int list -> int -> int list = <fun> *)
let factors n = factors_tail n [] 2;;
(* val factors : int -> int list = <fun> *)
factors 315;;
(* - : int list = [7; 5; 3; 3] *)
let factors n = List.rev (factors_tail n [] 2);;
(* val factors : int -> int list = <fun> *)
factors 315;;
(* - : int list = [3; 3; 5; 7] *)
factors 2;;
(* - : int list = [2] *)
factors 1;;
(* - : int list = [] *)
factors 20;;
(* - : int list = [2; 2; 5] *)
