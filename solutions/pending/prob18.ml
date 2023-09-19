let rec split_tail l1 l2 n =
        match l2 with
        | [] -> (l1, [])
        | h :: t -> match n with
                | 0 -> (l1, l2)
                | _ -> split_tail (l1 @ [h]) t (n - 1);;
(* val split_tail :
  'a list -> 'a list -> int -> 'a list * 'a list = <fun> *)
let split l n = split_tail [] l n;;
(* val split : 'a list -> int -> 'a list * 'a list = <fun> *)
let slice l i k =
let (end_removed, _) = split l (k + 1) in
end_removed;;
(* val slice : 'a list -> 'b -> int -> 'a list = <fun> *)
let slice l i k =
let (end_removed, _) = split l (k + 1) in
let (_, begin_removed) = split end_removed (i + 1) in
begin_removed;;
(* val slice : 'a list -> int -> int -> 'a list = <fun> *)
slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
(* - : string list = ["d"; "e"; "f"; "g"] *)
let slice l i k =
let (end_removed, _) = split l (k + 1) in
let (_, begin_removed) = split end_removed i in
begin_removed;;
(* val slice : 'a list -> int -> int -> 'a list = <fun> *)
slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
(* - : string list = ["c"; "d"; "e"; "f"; "g"] *)
