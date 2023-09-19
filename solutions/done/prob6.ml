let rec rev_tail l res =
        match l with
        | [] -> res
        | h :: t -> rev_tail t (h :: res);;
let rev l = rev_tail l [];;

let is_palindrome l = l = (rev l);;

(* TEST CASES:

is_palindrome ["x"; "a"; "m"; "a"; "x"];;
not (is_palindrome ["a"; "b"]);;

*)
