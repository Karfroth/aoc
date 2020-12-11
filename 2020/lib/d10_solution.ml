module IntMap = Map.Make(Int)

let solve_q1 = function
| [] -> -1
| (h :: t) as lst ->
  let rec aux (d1, d3) = function
  | h1 :: h2 :: t ->
    let diff = h2 - h1 in
    if diff = 1 then aux (d1 + 1, d3) (h2 :: t)
    else if diff = 3 then aux (d1, d3 + 1) (h2 :: t)
    else aux (d1, d3) (h2 :: t)
  | _ -> (d1, d3)
  in
  lst
  |> List.sort (fun x y -> x - y)
  |> aux (1, 1)
  |> fun (x, y) -> x * y

let build_dict lst =
  let rec aux acc = function
  | [] -> acc
  | h :: t ->
    aux (t |> List.filter (fun x -> x - h <= 3 && x - h > 0) |> fun x -> IntMap.add h x acc) t
  in
  aux IntMap.empty lst

let find_max lst =
  List.fold_left (fun acc x -> if acc > x then acc else x) 0 lst

let rec calc init target_num dict =
  let cache = Hashtbl.create (IntMap.cardinal dict) in
  let rec aux i =
    if i = target_num then 1
    else match Hashtbl.find_opt cache i with
    | Some v -> v
    | None ->
      match IntMap.find_opt i dict with
      | None -> 0
      | Some is ->
        let v = is |> List.map (fun x -> aux x) |> List.fold_left (fun acc x -> acc + x) 0 in
        Hashtbl.add cache i v;
        v
  in
  aux init

let preprocess_lst = function
| [] -> []
| h :: t -> 0 :: h :: t @ [(find_max t) + 3]
  
let solve_q2 lst =
  lst
  |> List.sort (fun x y -> x - y)
  |> preprocess_lst
  |> build_dict
  |> calc 0 (find_max lst)

let print_answer () =
  print_newline ();
  print_string "D10 Test1 for Q1: ";
  D10_input.test_input |> List.map int_of_string |> solve_q1 |> print_int; (* 35 *)
  print_newline ();
  print_string "D10 Test2 for Q1: ";
  D10_input.test_input2 |> List.map int_of_string |> solve_q1 |> print_int; (* 220 *)
  print_newline ();
  print_string "D10 Q1: ";
  D10_input.input1 |> List.map int_of_string |> solve_q1 |> print_int; (* 1890 *)
  print_newline ();
  print_string "D10 Test1 for Q2: ";
  D10_input.test_input |> List.map int_of_string |> solve_q2 |> print_int; (* 8 *)
  print_newline ();
  print_string "D10 Test2 for Q2: ";
  D10_input.test_input2 |> List.map int_of_string |> solve_q2 |> print_int; (* 19208 *)
  print_newline ();
  print_string "D10 Q2: ";
  D10_input.input1 |> List.map int_of_string |> solve_q2 |> print_int; (* 49607173328384 *)
  print_newline ();