(* let rec gcd m n =
if m = n then m
else if m > n then gcd (m - n) n
else gcd gcd (n - m) m;; *)
(* Error: This function has type int -> int -> int
       It is applied to too many arguments; maybe you forgot a `;'. *)
let rec gcd m n =
if m = n then m
else if m > n then gcd (m - n) n
else gcd (n - m) m;;
(* val gcd : int -> int -> int = <fun> *)
gcd 13 27;;
(* - : int = 1 *)
gcd 20536 7826;;
(* - : int = 2 *)
gcd 42 13;;
(* - : int = 1 *)
gcd 42 14;;
(* - : int = 14 *)
gcd 42 35;;
(* - : int = 7 *)
