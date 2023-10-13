let rec gcd m n =
if m = n then m
else if m > n then gcd (m - n) n
else gcd (n - m) m;;
(* val gcd : int -> int -> int = <fun> *)
let coprime m n =
gcd m n = 1;;
(* val coprime : int -> int -> bool = <fun> *)
let rec range a b = 
if a = b then [a] 
else if a < b then a :: range (a + 1) b 
else a :: range (a - 1) b;;
(* val range : int -> int -> int list = <fun> *)
let phi m =
List.fold_left (fun acc r -> acc + Bool.to_int (coprime r m)) 0 (range 1 (m-1));;
(* val phi : int -> int = <fun> *)
phi 10;;
(* - : int = 4 *)
phi 13;;
(* - : int = 12 *)
(* if m prime, phi(m) = m-1. *);;
phi 1;;
(* Interrupted. *)
phi 2;;
(* - : int = 1 *)
let phi m = 
if m = 1 then 1
else List.fold_left (fun acc r -> acc + Bool.to_int (coprime r m)) 0 (range 1 (m-1));;
(* val phi : int -> int = <fun> *)
