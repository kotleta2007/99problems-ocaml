type bool_expr = 
	| Var of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or  of bool_expr * bool_expr;;

let table2 var1 var2 expr = 
	let rec eval val1 val2 expression = 
		match expression with
		| Var v when v = var1 -> val1
		| Var v when v = var2 -> val2
		| Var v -> failwith "Unknown variable name."
		| Not e -> not (eval val1 val2 e)
		| And (e1, e2) -> (eval val1 val2 e1) && (eval val1 val2 e2)
		| Or (e1, e2) -> (eval val1 val2 e1) || (eval val1 val2 e2)
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
