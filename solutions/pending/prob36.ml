let rec gcd m n =
if m = n then m
else if m > n then gcd (m - n) n
else gcd (n - m) m;;
(* val gcd : int -> int -> int = <fun> *)
let coprime m n =
gcd m n = 1;;
(* val coprime : int -> int -> bool = <fun> *)
let rec factors_buf l n count k =
if n = 1 then l
else if (coprime n k)
then factors_buf ((k, count) :: l) n 0 (k + 1)
else factors_buf l (n / k) (count + 1) k;;
(* val factors_buf :
  (int * int) list -> int -> int -> int -> (int * int) list =
  <fun> *)
let rec factors_as_pairs n k =
if n = 1 then []
else if (coprime n k) then factors_as_pairs n (k + 1)
else factors_buf [] (n / k) 1 k;;
(* val factors_as_pairs : int -> int -> (int * int) list = <fun> *)
let factors n =
List.rev (factors_as_pairs n 2);;
(* val factors : int -> (int * int) list = <fun> *)
factors 315;;
(* - : (int * int) list = [(3, 2); (4, 0); (5, 1); (6, 0)] *)
let rec factors_buf l n count k =
if n = 1 then (k, count) :: l
else if (coprime n k)
then factors_buf ((k, count) :: l) n 0 (k + 1)
else factors_buf l (n / k) (count + 1) k;;
(* val factors_buf :
  (int * int) list -> int -> int -> int -> (int * int) list =
  <fun> *)
factors 315;;
(* - : (int * int) list = [(3, 2); (4, 0); (5, 1); (6, 0)] *)
let rec factors_buf l n count k =
if n = 1 then ((k, count) :: l)
else if (coprime n k)
then factors_buf ((k, count) :: l) n 0 (k + 1)
else factors_buf l (n / k) (count + 1) k;;
(* val factors_buf :
  (int * int) list -> int -> int -> int -> (int * int) list =
  <fun> *)
factors 315;;
(* - : (int * int) list = [(3, 2); (4, 0); (5, 1); (6, 0)] *)
let rec factors_as_pairs n k =
if n = 1 then []
else if (coprime n k) then factors_as_pairs n (k + 1)
else factors_buf [] (n / k) 1 k;;
(* val factors_as_pairs : int -> int -> (int * int) list = <fun> *)
let factors n =
List.rev (factors_as_pairs n 2);;
(* val factors : int -> (int * int) list = <fun> *)
factors 315;;
(* - : (int * int) list = [(3, 2); (4, 0); (5, 1); (6, 0); (7, 1)] *)
let rec factors_buf l n count k =
if n = 1 then ((k, count) :: l)
else if (coprime n k)
then 
if count = 0 then
factors_buf l n 0 (k + 1)
else
factors_buf ((k, count) :: l) n 0 (k + 1)
else factors_buf l (n / k) (count + 1) k;;
(* val factors_buf :
  (int * int) list -> int -> int -> int -> (int * int) list =
  <fun> *)
factors 315;;
(* - : (int * int) list = [(3, 2); (4, 0); (5, 1); (6, 0); (7, 1)] *)
let rec factors_as_pairs n k =
if n = 1 then []
else if (coprime n k) then factors_as_pairs n (k + 1)
else factors_buf [] (n / k) 1 k;;
(* val factors_as_pairs : int -> int -> (int * int) list = <fun> *)
let factors n =
List.rev (factors_as_pairs n 2);;
(* val factors : int -> (int * int) list = <fun> *)
factors 315;;
(* - : (int * int) list = [(3, 2); (5, 1); (7, 1)] *)
