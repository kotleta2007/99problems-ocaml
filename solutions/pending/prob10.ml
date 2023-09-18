let rec rev_tail l res =
        match l with
        | [] -> res
        | h :: t -> rev_tail t (h :: res);;
(* val rev_tail : 'a list -> 'a list -> 'a list = <fun> *)
let rev l = rev_tail l [];;
(* val rev : 'a list -> 'a list = <fun> *)
let rec pack_tail l1 l2 =
        match l1 with
        | [] -> l2
        | h1 :: t1 -> match l2 with
        | [] -> pack_tail t1 ([h1] :: [])
        | h2 :: t2 -> 
                if h1 = (List.hd h2) 
                then (pack_tail t1 ([h1 :: h2] @ t2)) 
                else (pack_tail t1 ([h1] :: h2 :: t2));;
(* val pack_tail : 'a list -> 'a list list -> 'a list list = <fun> *)
let pack l = rev (pack_tail l []);;
(* val pack : 'a list -> 'a list list = <fun> *)
(* let rec encode l = (pack l) match
| [] -> []
| h :: t -> (List.length h, List.hd h) :: (encode t);; *)
(* Error: Syntax error *)
(* let rec encode l = match
| [] -> []
| h :: t -> (List.length h, List.hd h) :: (encode t);; *)
(* Error: Syntax error *)
(* let rec encode l = 
match (pack l) with
| [] -> []
| h :: t -> (List.length h, List.hd h) :: (encode t);; *)
(* Error: This expression has type 'a list list
       but an expression was expected of type 'a list
       The type variable 'a occurs inside 'a list *)
(* let rec encode l = 
match (pack l) with
| [] -> []
| h :: t -> (List.length h, List.hd h) @ (encode t);; *)
(* Error: This expression has type 'a * 'b but an expression was expected of type
         'c list *)
(* let rec encode l = 
match (pack l) with
| [] -> []
| h :: t -> (List.length h, List.hd h) :: (encode t);; *)
(* Error: This expression has type 'a list list
       but an expression was expected of type 'a list
       The type variable 'a occurs inside 'a list *)
(* let rec encode l = 
match (pack l) with
| [] -> []
| h :: t -> h :: (encode t);; *)
(* Error: This expression has type 'a list list
       but an expression was expected of type 'a list
       The type variable 'a occurs inside 'a list *)
(* let rec encode l = 
match (pack l) with
| [] -> []
| h :: t -> [h] :: (encode t);; *)
(* Error: This expression has type 'a list list
       but an expression was expected of type 'a list
       The type variable 'a occurs inside 'a list *)
(* let rec encode_tail l = 
match l with
| [] -> []
| h :: t -> (List.length h, List.hd h) :: (encode t);; *)
(* Error: Unbound value encode *)
let rec encode_tail l = 
match l with
| [] -> []
| h :: t -> (List.length h, List.hd h) :: (encode_tail t);;
(* val encode_tail : 'a list list -> (int * 'a) list = <fun> *)
(* let encode = encode_tail (pack l);; *)
(* Error: Unbound value l *)
let encode l = encode_tail (pack l);;
(* val encode : 'a list -> (int * 'a) list = <fun> *)
encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)
