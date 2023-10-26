(* let rec table l = 
match l with
| [] -> []
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x) rest
@
List.map (fun x -> (h, false :: x) rest;; *)
(* Error: Syntax error: ')' expected, the highlighted '(' might be unmatched *)
(* let rec table l = 
match l with
| [] -> []
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x)) rest
@
List.map (fun x -> (h, false :: x)) rest;; *)
(* Error: Syntax error *)
(* let rec table l = 
match l with
| [] -> []
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x)) rest
@
List.map (fun x -> (h, false :: x)) rest;; *)
(* Error: Syntax error *)
(* let rec table l = 
match l with
| [] -> []
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x) rest
@
List.map (fun x -> (h, false :: x) rest;; *)
(* Error: Syntax error: ')' expected, the highlighted '(' might be unmatched *)
(* let rec table l = 
match l with
| [] -> []
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x) rest @ List.map (fun x -> (h, false :: x) rest;; *)
(* Error: Syntax error: ')' expected, the highlighted '(' might be unmatched *)
(* let rec table l = 
match l with
| [] -> []
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x) rest @ List.map (fun x -> (h, false :: x) rest;; *)
(* Error: Syntax error: ')' expected, the highlighted '(' might be unmatched *)
(* let rec table l = 
match l with
| [] -> []
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x) rest 
@ 
List.map (fun x -> (h, false :: x) rest;; *)
(* Error: Syntax error: ')' expected, the highlighted '(' might be unmatched *)
let rec table l = 
match l with
| [] -> []
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x) rest;;
(* val table : 'a list -> ('a * bool) list list = <fun> *)
table ["a"; "b"; "c"];;
(* - : (string * bool) list list = [] *)
let rec table l = 
match l with
| [] -> [[]]
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x) rest;;
(* val table : 'a list -> ('a * bool) list list = <fun> *)
table ["a"; "b"; "c"];;
(* - : (string * bool) list list =
[[("a", true); ("b", true); ("c", true)]] *)
let rec table l = 
match l with
| [] -> [[]]
| h :: t -> 
let rest = table t in
List.map (fun x -> (h, true) :: x) rest
@
List.map (fun x -> (h, false) :: x) rest;;
(* val table : 'a list -> ('a * bool) list list = <fun> *)
table ["a"; "b"; "c"];;
(* - : (string * bool) list list =
[[("a", true); ("b", true); ("c", true)];
 [("a", true); ("b", true); ("c", false)];
 [("a", true); ("b", false); ("c", true)];
 [("a", true); ("b", false); ("c", false)];
 [("a", false); ("b", true); ("c", true)];
 [("a", false); ("b", true); ("c", false)];
 [("a", false); ("b", false); ("c", true)];
 [("a", false); ("b", false); ("c", false)]] *)
let rec blank_table l = 
match l with
| [] -> [[]]
| h :: t -> 
let rest = blank_table t in
List.map (fun x -> (h, true) :: x) rest
@
List.map (fun x -> (h, false) :: x) rest;;
(* val blank_table : 'a list -> ('a * bool) list list =
  <fun> *)
type bool_expr =
| Var of string
| Not of bool_expr
| And of bool_expr * bool_expr
| Or of bool_expr * bool_expr;;
(* type bool_expr =
    Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr *)
(* let lookup table_entry x = 
match table_entry with
| (var, val) :: t when var = x -> val
| h :: t -> lookup t x;; *)
(* Error: Syntax error: pattern expected. *)
(* let lookup table_entry x = 
match table_entry with
| (var, value) :: t when var = x -> value
| h :: t -> lookup t x;; *)
(* Error: Unbound value lookup
Hint: If this is a recursive definition,
you should add the 'rec' keyword on line 1 *)
let rec lookup table_entry x = 
match table_entry with
| (var, value) :: t when var = x -> value
| h :: t -> lookup t x;;
(* val lookup : ('a * 'b) list -> 'a -> 'b = <fun> *)
let rec eval table_entry expr =
match expr with
| Var x -> lookup table_entry x
| Not x -> not (eval table_entry x)
| And (x, y) -> (eval table_entry x) && (eval table_entry y)
| Or (x, y) -> (eval table_entry x) || (eval table_entry y);;
(* val eval : (string * bool) list -> bool_expr -> bool = <fun> *)
(* let table vars expr =
let table_entries = blank_table vars in
List.map 
(fun x -> (x, (eval x expr))
table_entries;; *)
(* Error: Syntax error: ')' expected, the highlighted '(' might be unmatched *)
let table vars expr =
let table_entries = blank_table vars in
List.map 
(fun x -> (x, (eval x expr)))
table_entries;;
(* val table :
  string list -> bool_expr -> ((string * bool) list * bool) list =
  <fun> *)
table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;
(* - : ((string * bool) list * bool) list =
[([("a", true); ("b", true)], true);
 ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false);
 ([("a", false); ("b", false)], false)] *)
let a = Var "a" and b = Var "b" and c = Var "c" in table ["a"; "b"; "c"] (Or (And (a, Or (b,c)), Or (And (a,b), And (a,c))));;
(* - : ((string * bool) list * bool) list =
[([("a", true); ("b", true); ("c", true)], true);
 ([("a", true); ("b", true); ("c", false)], true);
 ([("a", true); ("b", false); ("c", true)], true);
 ([("a", true); ("b", false); ("c", false)], false);
 ([("a", false); ("b", true); ("c", true)], false);
 ([("a", false); ("b", true); ("c", false)], false);
 ([("a", false); ("b", false); ("c", true)], false);
 ([("a", false); ("b", false); ("c", false)], false)] *)
