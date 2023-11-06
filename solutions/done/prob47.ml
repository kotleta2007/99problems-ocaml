type bool_expr = 
	| Var of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or  of bool_expr * bool_expr
	| Impl of bool_expr * bool_expr
	| Eq of bool_expr * bool_expr
	| Nand of bool_expr * bool_expr
	| Nor of bool_expr * bool_expr
	| Xor of bool_expr * bool_expr;;

(* 
	Unlike other functional languages, 
	OCaml doesn't support infix operators
	with alphanumeric identifier names.

	Therefore, the solution of P47
	is a copy of the solution of P46.

	This program features other
	logical connectives (e.g. NAND and NOR)
	found in the 99 Lisp Problems.
*)

let table2 var1 var2 expr = 
	let rec eval val1 val2 expression = 
		match expression with
		| Var v when v = var1 -> val1
		| Var v when v = var2 -> val2
		| Var v -> failwith "Unknown variable name."
		| Not e -> not (eval val1 val2 e)
		| And (e1, e2) -> (eval val1 val2 e1) && (eval val1 val2 e2)
		| Or (e1, e2) -> (eval val1 val2 e1) || (eval val1 val2 e2)
		| Impl (e1, e2) -> (eval val1 val2 (Or ((Not e1), e2)))
		| Eq (e1, e2) -> (eval val1 val2 (And ((Impl (e1, e2)), (Impl (e2, e1)))))
		| Nand (e1, e2) -> (eval val1 val2 (Not (And (e1, e2))))
		| Nor (e1, e2) -> (eval val1 val2 (Not (Or (e1, e2))))
		| Xor (e1, e2) -> (eval val1 val2 (And ((Or (e1, e2)), (Not (And (e1, e2))))))
	in
	let vals = 
		[(true, true); (true, false); (false, true); (false, false)]
	in
	List.map
	(fun p -> ((fst p), (snd p), eval (fst p) (snd p) expr))
	vals;;

(* TEST CASES:

table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;

*)
