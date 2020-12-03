let find lst =
  let rec aux = function
  | [] -> None
  | (h :: t) as l ->
    match List.find_opt (fun x -> (x + h) = 2020) l with
    | None -> aux t
    | v -> Option.map (fun x -> x * h) v
  in
  aux lst

let rec find2 lst =
  let rec aux v = function
  | [] -> None
  | (h :: t) as l ->
    match List.find_opt (fun x -> (v + x + h) = 2020) l with
    | None -> aux v t
    | a -> Option.map (fun x -> x * v * h) a
  in
  match lst with
  | [] -> None
  | h :: t ->
    match aux h t with
    | None -> find2 t
    | a -> a

let to_string = function
| None -> "No Answer"
| Some v -> string_of_int v

let print_answer () =
  print_newline ();
  print_string "D1 Test Input: ";
  D1_Input.test_input |> find |> to_string |> print_string;
  print_newline ();
  print_string "D1 Q1: ";
  D1_Input.input1 |> find |> to_string |> print_string; (* 618144 *)
  print_newline ();
  print_string "D1 Q2: ";
  D1_Input.input1 |> find2 |> to_string |> print_string; (* 173538720 *)
  print_newline ();