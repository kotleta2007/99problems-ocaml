let rec gcd m n =
if m = n then m
else if m > n then gcd (m - n) n
else gcd (n - m) m;;
(* val gcd : int -> int -> int = <fun> *)
let coprime m n =
gcd m n = 1;;
(* val coprime : int -> int -> bool = <fun> *)
coprime 13 27;;
(* - : bool = true *)
not (coprime 20536 7826);;
(* - : bool = true *)
