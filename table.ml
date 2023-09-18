(* DONE *)

let solutions_dir_done = String.cat (Sys.getcwd()) "/solutions/done";;
let pat = Str.regexp {|prob\([0-9]+\).ml|};;
let filename2link_done s = String.concat "" ["[☑](https://github.com/kotleta2007/99problems-ocaml/blob/main/solutions/done/"; s; ")"];;
let to_tuple_done s = (Stdlib.int_of_string (Str.replace_first pat {|\1|} s), filename2link_done(s));;

let done_ = Sys.readdir (solutions_dir_done)
|> Array.to_list
|> List.filter (fun x -> Filename.extension x = ".ml")
|> List.filter (fun x -> String.starts_with ~prefix:"prob" x)
|> List.map to_tuple_done;;

(* PENDING *)

let solutions_dir_pending = String.cat (Sys.getcwd()) "/solutions/pending";;
let filename2link_pending s = String.concat "" ["[☒](https://github.com/kotleta2007/99problems-ocaml/blob/main/solutions/pending/"; s; ")"];;
let to_tuple_pending s = (Stdlib.int_of_string (Str.replace_first pat {|\1|} s), filename2link_pending(s));;

let pending = Sys.readdir (solutions_dir_pending)
|> Array.to_list
|> List.filter (fun x -> Filename.extension x = ".ml")
|> List.filter (fun x -> String.starts_with ~prefix:"prob" x)
|> List.map to_tuple_pending;;

(* SPECIAL *)

let special = []
|> List.cons (0, "☺");;

(* RES *)

let res = done_ @ pending @ special
|> List.sort (fun x y -> compare (fst x) (fst y));;

Printf.printf "Table entries: \n";;
List.iter (fun x -> Printf.printf "%d, %s\n" (fst x) (snd x)) res;;

let tableEntry i j = 
	try (snd (List.find (fun x -> (fst x) = (10*i + j)) res)) with
	Not_found -> "";;
let file = "README.md"

let () = 
	let oc = open_out file in
	Printf.fprintf oc "# 99problems-ocaml\n";
	Printf.fprintf oc "[99 Problems](https://v2.ocaml.org/learn/tutorials/99problems.html), solved in [OCaml](https://ocaml.org/)\n";
	Printf.fprintf oc "\n#\n\n";

	Printf.fprintf oc "|  | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |\n";
	Printf.fprintf oc "|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|---|\n";

	for i = 0 to 9 do
		Printf.fprintf oc "| %d0 |" i;
		for j = 0 to 9 do
			Printf.fprintf oc " %s |" (tableEntry i j)
		done;
		Printf.fprintf oc "\n"
	done;
	close_out oc;;

Printf.printf "\n☺\n";;
