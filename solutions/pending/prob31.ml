(* Float.sqrt(5);; *)
(* Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `5.'? *)
Float.sqrt(5.);;
(* - : float = 2.23606797749979 *)
Float.sqrt(float_of_int(5));;
(* - : float = 2.23606797749979 *)
Float.floor(Float.sqrt(float_of_int(5)));;
(* - : float = 2. *)
(* Float.to_int Float.floor(Float.sqrt(float_of_int(5)));; *)
(* Error: This function has type float -> int
       It is applied to too many arguments; maybe you forgot a `;'. *)
(* Float.to_int Float.floor Float.sqrt float_of_int 5;; *)
(* Error: This function has type float -> int
       It is applied to too many arguments; maybe you forgot a `;'. *)
(* Float.to_int (Float.floor Float.sqrt float_of_int 5);; *)
(* Error: This function has type float -> float
       It is applied to too many arguments; maybe you forgot a `;'. *)
(* Float.to_int (Float.floor (Float.sqrt float_of_int 5));; *)
(* Error: This function has type float -> float
       It is applied to too many arguments; maybe you forgot a `;'. *)
Float.to_int (Float.floor (Float.sqrt (float_of_int 5)));;
(* - : int = 2 *)
let int_sqrt n = Float.to_int (Float.floor (Float.sqrt (float_of_int n)));;
(* val int_sqrt : int -> int = <fun> *)
5 mod 2;;
(* - : int = 1 *)
(* mod 5 2;; *)
(* Error: Syntax error *)
let rec range a b = 
        if a = b then [a] 
        else if a < b then a :: range (a + 1) b 
        else a :: range (a - 1) b;;
(* val range : int -> int -> int list = <fun> *)
range 2 2;;
(* - : int list = [2] *)
(* use fold_left_map with booleans *);;
