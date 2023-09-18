let rec rev_tail l res =
        match l with
        | [] -> res
        | h :: t -> rev_tail t (h :: res);;
(* val rev_tail : 'a list -> 'a list -> 'a list = <fun> *)
let rev l = rev_tail l [];;
(* val rev : 'a list -> 'a list = <fun> *)
[1; 2] = [1; 2];;
(* - : bool = true *)
let is_palindrome l = l = (rev l);;
(* val is_palindrome : 'a list -> bool = <fun> *)
is_palindrome ["x"; "a"; "m"; "a"; "x"];;
(* - : bool = true *)
(* not (is_palindrome ["a"; "b"];; *)
(* Error: Syntax error: ')' expected, the highlighted '(' might be unmatched *)
not (is_palindrome ["a"; "b"]);;
(* - : bool = true *)
