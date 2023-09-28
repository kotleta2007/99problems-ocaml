Random.init 0;;

let rec range a b = 
        if a = b then [a] 
        else if a < b then a :: range (a + 1) b 
        else a :: range (a - 1) b;;

let rec split_tail l1 l2 n =
        match l2 with
        | [] -> (l1, [])
        | h :: t -> match n with
                | 0 -> (l1, l2)
                | _ -> split_tail (l1 @ [h]) t (n - 1);;
let split l n = split_tail [] l n;;

let remove_at k l =
        let (left, right) = split l k in
                match right with
                | [] -> left
                | h :: t -> left @ t;;

let rec rand_select l n =
        let rand_index = (Random.int (List.length l)) in
        let rand_elem = List.nth l rand_index in
        let remaining = remove_at rand_index l in
                if n = 1
                then [rand_elem]
                else rand_elem :: (rand_select remaining (n - 1));;

let lotto_select n m =
        let l = range 1 m in
        rand_select l n;;

let permutation l = rand_select l (List.length l);;

(* TEST CASES:

permutation ["a"; "b"; "c"; "d"; "e"; "f"];;

*)
