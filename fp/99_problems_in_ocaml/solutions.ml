let run_test_case (value: 'a) (expected: 'a) =
    match value = expected with
    | true -> Printf.printf "Testcase passed\n"
    | false -> Printf.printf "Testcase failed\n";;

(* Problem 01 *)
let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: rest -> last rest;;

print_endline "Problem #01";;
run_test_case (last ["a";"b";"c";"d"]) (Some "d");;
run_test_case (last []) (None);;

(* Problem 02 *)
let rec last_2 = function
    | [] | [_] -> None
    | [a;b] -> Some (a, b)
    | _ :: t -> last_2 t;;

print_endline "Problem #02";;
run_test_case (last_2 ["a"; "b"; "c"; "d"]) (Some("c", "d"));;
run_test_case (last_2 ["a"]) (None);;

(* Problem 03 *)
let rec at k = function
    | [] -> None
    | h :: t -> if k = 0 then Some h else at (k - 1) t;;

print_endline "Problem #03";;
run_test_case (at 2 ["a"; "b"; "c"; "d"; "e"]) (Some "c");;
run_test_case (at 2 ["a"]) (None);;

(* Problem 04 *)
let length list =
    let rec slide n = function
        | [] -> n
        | _ :: t -> slide (n + 1) t
    in slide 0 list;;

print_endline "Problem #04";;
run_test_case (length ["a"; "b"; "c"]) 3;;
run_test_case (length []) 0;;

(* Problem 05 *)
let reverse list =
    let rec aux acc = function
        | [] -> acc
        | h :: t -> aux (h :: acc) t         
    in
    aux [] list;;

print_endline "Problem #05";;
run_test_case (reverse ["a"; "b"; "c"]) ["c"; "b"; "a"]

(* Problem 06 *)
let is_palindrome word =
    word = reverse word;;

print_endline "Problem #06";;
run_test_case (is_palindrome ["x"; "a"; "m"; "a"; "x"]) true;;
run_test_case (is_palindrome ["a"; "b"]) false;;

(* Problem 07 *)
type 'a node =
    | One of 'a
    | Many of 'a node list;;

let flatten list =
    let rec aux acc = function
        | [] -> acc
        | One x :: t -> aux (x :: acc) t
        | Many l :: t -> aux (aux acc l) t
    in List.rev (aux [] list);;

print_endline "Problem #07";;
run_test_case (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]) (["a"; "b"; "c"; "d"; "e"]);;

(* Problem 08 *)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;

print_endline "Problem #08";;
run_test_case (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) (["a"; "b"; "c"; "a"; "d"; "e"]);;

(* Problem 09 *)
let pack list =
    let rec aux current acc = function
        | [] -> []
        | [x] -> (x :: current) :: acc
        | a :: (b :: _ as t) -> 
            if a = b then aux (a :: current) acc t
            else aux [] ((a :: current) :: acc) t
    in List.rev (aux [] [] list);;

print_endline "Problem #09";;
run_test_case (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"])
([["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]);;

(* Problem 10 *)
(* let encode list =
    let rec aux = function
        | [] -> []
        | (x ::_ as a) :: t -> (List.length a, x) :: aux t
        | _ -> raise (Failure "Not possible")
    in aux (pack list);;
*)
let encode list = List.map (fun l -> (List.length l, List.hd l)) (pack list);;

print_endline "Problem #10";;
run_test_case (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")];;

(* Problem 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode list =
    List.map (fun l -> (if (List.length l) > 1 then Many(List.length l, List.hd l) else One (List.hd l))) (pack list);;

print_endline "Problem #11";;
run_test_case (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]

(* Problem 12 *)
let decode list =
    let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x 
in
    let rec aux acc = function
        | [] -> acc
        | One x :: t -> aux (x :: acc) t
        | Many (n, x) :: t -> aux (many acc n x) t
in
    aux [] (List.rev list);;

print_endline "Problem #12";;
run_test_case (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")])
(["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);;

(* Problem 13 *)
let encode list =
    let rle count x = if count = 0 then One x else Many (count + 1, x)
in
    let rec aux count acc = function
        | [] -> []
        | [x] -> rle count x :: acc
        | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                                else aux 0 (rle count a :: acc) t
in
    List.rev (aux 0 [] list);;

print_endline "Problem #13";;
run_test_case (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
([Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")])

(* Problem 14 *)
let rec duplicate = function
    | [] -> [] 
    | a :: t -> a :: a :: duplicate t;;

print_endline "Problem #14";;
run_test_case (duplicate ["a"; "b"; "c"; "c"; "d"])
(["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])

(* Problem 15 *)
(* let replicate list n = 
    let rec expand n acc x = if n = 0 then acc else expand (n - 1) (x :: acc) x
in
    let rec aux acc = function
        | [] -> acc
        | a :: t -> aux (expand n acc a) t
in
    List.rev (aux [] list);;*)

let replicate list n =
    let rec prepend n acc x =
        if n = 0 then acc else prepend (n - 1) (x :: acc) x
in
    List.fold_left (prepend n) [] (List.rev list);;

print_endline "Problem #15";;
run_test_case (replicate ["a"; "b"; "c"] 3)
(["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]);;

(* Problem 16 *)
let drop list n =
    let rec aux i = function
        | [] ->[]
        | h :: t -> if i = n then aux 1 t else h :: aux (i + 1) t in 
    aux 1 list;;

print_endline "Problem #16";;
run_test_case (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3)
(["a"; "b"; "d"; "e"; "g"; "h"; "j"]);;

(* Problem 17 *)
let split list n =
    let rec aux i acc = function
        | [] -> List.rev acc, []
        | h :: t as l -> if i = 0 then List.rev acc, l
                         else aux (i - 1) (h :: acc) t
in
    aux n [] list;;

print_endline "Problem #17";;
run_test_case (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3)
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]);;

(* Problem 18 *)
let slice list i k =
    let rec take n = function
        | [] -> []
        | h :: t -> if n = 0 then [] else h :: take (n - 1) t
in
    let rec drop n = function
        | [] -> []
        | h :: t as l -> if n = 0 then l else drop (n - 1) t
in
    take (k - i + 1) (drop i list);;

print_endline "Problem #18";;
run_test_case (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6)
(["c"; "d"; "e"; "f"; "g"]);;

(* Problem 19 *)
let rotate list n =
    let len = List.length list in
    let n = if len = 0 then 0 else n mod len in
    if n = 0 then list else let a, b = split list n in b @ a;;

print_endline "Problem #19";;
run_test_case (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3)
(["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"])

(* Problem 20 *)
let rec remove_at i = function
    | [] -> []
    | a :: t -> if i = 0 then t else a :: remove_at (i - 1) t;;

print_endline "Problem #20";;
run_test_case (remove_at 1 ["a"; "b"; "c"; "d"])
["a"; "c"; "d"];;

(* Problem 21 *)