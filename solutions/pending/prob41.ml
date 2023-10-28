let rec range a b = 
if a <= b then a :: range (a + 1) b 
else [];;
(* val range : int -> int -> int list = <fun> *)
let is_prime n = 
let int_sqrt n = 
int_of_float 
(Float.floor 
(Float.sqrt 
(float_of_int n))) in
if n = 1 then false 
else List.fold_left 
(fun acc k -> acc && (n mod k != 0)) 
true 
(range 2 (int_sqrt n));;
(* val is_prime : int -> bool = <fun> *)
let all_primes a b =
let l = range a b in
List.filter is_prime l;;
(* val all_primes : int -> int -> int list = <fun> *)
let goldbach n =
if n mod 2 = 1 || n < 4 then (-1, -1)
else let primes = all_primes 2 n in
let rec iter l = 
match l with
| [] -> (-1, -1)
| h :: t -> 
let complement = n - h in
if (is_prime complement) then (h, complement)
else iter t in 
iter primes;;
(* val goldbach : int -> int * int = <fun> *)
let goldbach_list a b =
let nums = range a b in
let rec iter l =
match l with
| [] -> []
| h :: t -> 
let g = goldbach h in
match g with
| (-1, -1) -> iter t
| _ -> g :: iter t in
iter nums;;
(* val goldbach_list : int -> int -> (int * int) list = <fun> *)
goldbach_list 9 20;;
(* - : (int * int) list = [(3, 7); (5, 7); (3, 11); (3, 13); (5, 13); (3, 17)] *)
let goldbach_list a b =
let nums = range a b in
let rec iter l =
match l with
| [] -> []
| n :: rest -> 
let goldbach_pair = goldbach n in
match goldbach_pair with
| (-1, -1) -> iter rest
| _ -> (n, goldbach_pair) :: iter rest in
iter nums;;
(* val goldbach_list : int -> int -> (int * (int * int)) list = <fun> *)
goldbach_list 9 20;;
(* - : (int * (int * int)) list =
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))] *)
(* let goldbach_limit a b bound = 
let l = goldbach_list a b in
List.filter
(fun x -> snd fst x)
l;; *)
(* Error: This expression has type 'a * 'b -> 'a
       but an expression was expected of type 'c * ('d -> 'e) *)
(* let goldbach_limit a b bound = 
let l = goldbach_list a b in
List.filter
(fun x -> snd (fst x))
l;; *)
(* Error: This expression has type (int * (int * int)) list
       but an expression was expected of type (('a * bool) * 'b) list
       Type int is not compatible with type 'a * bool *)
goldbach_list 9 20;;
(* - : (int * (int * int)) list =
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))] *)
(* List.hd goldbach_list 9 20;; *)
(* Error: This expression has type int -> int -> (int * (int * int)) list
       but an expression was expected of type ('a -> 'b -> 'c) list *)
List.hd (goldbach_list 9 20);;
(* - : int * (int * int) = (10, (3, 7)) *)
(* let goldbach_limit a b bound = 
let l = goldbach_list a b in
List.filter
(fun x -> fst (snd x))
l;; *)
(* Error: This expression has type (int * (int * int)) list
       but an expression was expected of type (int * (bool * 'a)) list
       Type int is not compatible with type bool *)
let goldbach_limit a b bound = 
let l = goldbach_list a b in
List.map
(fun x -> fst (snd x))
l;;
(* val goldbach_limit : int -> int -> 'a -> int list = <fun> *)
(* goldbach_list 1 2000 50;; *)
(* Error: This function has type int -> int -> (int * (int * int)) list
       It is applied to too many arguments; maybe you forgot a `;'. *)
goldbach_limit 1 2000 50;;
(* - : int list =
[2; 3; 3; 3; 5; 3; 3; 5; 3; 3; 5; 3; 5; 7; 3; 3; 5; 7; 3; 5; 3; 3; 5; 3; 5; 7;
 3; 5; 7; 3; 3; 5; 7; 3; 5; 3; 3; 5; 7; 3; 5; 3; 5; 7; 3; 5; 7; 19; 3; 5; 3; 3;
 5; 3; 3; 5; 3; 5; 7; 13; 11; 13; 19; 3; 5; 3; 5; 7; 3; 3; 5; 7; 11; 11; 3; 3;
 5; 7; 3; 5; 7; 3; 5; 3; 5; 7; 3; 5; 7; 3; 3; 5; 7; 11; 11; 3; 3; 5; 3; 3; 5;
 7; 11; 11; 13; 3; 5; 7; 23; 11; 13; 3; 5; 3; 3; 5; 3; 5; 7; 3; 3; 5; 7; 11;
 11; 3; 5; 7; 3; 5; 7; 3; 5; 7; 3; 3; 5; 7; 3; 5; 3; 3; 5; 7; 11; 11; 3; 5; 7;
 19; 11; 13; 31; 3; 5; 3; 3; 5; 3; 5; 7; 13; 11; 13; 19; 3; 5; 7; 3; 5; 7; 29;
 11; 3; 3; 5; 3; 5; 7; 3; 5; 7; 19; 3; 5; 7; 3; 5; 7; 3; 5; 3; 5; 7; 3; 5; 7;
 19; 3; 5; 3; 5; 7; 13; 3; 5; 7; 17; 11; 3; 3; 5; 7; 11; 11; 3; 3; 5; 7; 3; 5;
 3; 5; 7; 3; 5; 7; 19; 3; 5; 3; 3; 5; 3; 5; 7; 13; 11; 13; 3; 5; 7; 31; 3; 5;
 3; 5; 7; 13; 3; 5; 3; 5; 7; 3; 5; 7; 19; 11; 13; 3; 3; 5; 7; 11; 11; 13; 17;
 17; 19; 3; 5; 7; 3; 5; 7; 47; 11; 3; 5; 7; 3; 5; 7; 3; 3; 5; 7; 3; 5; 7; 17;
 11; 3; 5; 7; 3; 5; 7; ...] *)
let goldbach_limit a b bound = 
let l = goldbach_list a b in
List.filter
(fun x -> fst (snd x) > bound)
l;;
(* val goldbach_limit : int -> int -> int -> (int * (int * int)) list = <fun> *)
goldbach_limit 1 2000 50;;
(* - : (int * (int * int)) list =
[(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789)); (1928, (61, 1867))] *)
goldbach_limit 2 3000 50;;
(* - : (int * (int * int)) list =
[(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789)); (1928, (61, 1867));
 (2078, (61, 2017)); (2438, (61, 2377)); (2512, (53, 2459));
 (2530, (53, 2477)); (2618, (61, 2557)); (2642, (103, 2539))] *)
List.length (goldbach_limit 2 3000 50);;
(* - : int = 10 *)
