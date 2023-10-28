let rec gcd m n =
	if m = n then m
	else if m > n then gcd (m - n) n
	else gcd (n - m) m;;
let coprime m n =
	gcd m n = 1;;

let rec factors_buf l n count k =
	if n = 1 then ((k, count) :: l)
	else if (coprime n k) then 
		if count = 0 then factors_buf l n 0 (k + 1)
		else factors_buf ((k, count) :: l) n 0 (k + 1)
	else factors_buf l (n / k) (count + 1) k;;
let rec factors_as_pairs n k =
	if n = 1 then []
	else if (coprime n k) then factors_as_pairs n (k + 1)
	else factors_buf [] (n / k) 1 k;;
let factors n =
	List.rev (factors_as_pairs n 2);;

let rec pow_tail a n res =
	match n with
	| 0 -> res
	| 1 -> (a * res)
	| k when k mod 2 = 0 -> pow_tail (a * a) (k / 2) res
	| k -> pow_tail (a * a) (k / 2) (a * res);;
let pow a n = pow_tail a n 1;;

let phi_improved n =
	let l = factors n in
	List.fold_left
		(fun acc x ->
			match x with
			| (p, m) -> acc * (p - 1) * (pow p (m - 1))
		) 1 l;;

(* TEST CASES:

phi_improved 10;;
phi_improved 13;;

*)
