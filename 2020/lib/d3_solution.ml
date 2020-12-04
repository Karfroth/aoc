let next_x x_limit delta_x x =
  let x_candidate = x + delta_x in
  if x_candidate >= x_limit then x_candidate - x_limit else x_candidate

let string_get_opt str i =
  try Some (String.get str i)
  with _ -> None

let lst_drop_opt n lst = 
  let rec aux i lst =
    if i = 0 then Some lst
    else match lst with
    | [] -> None
    | _ :: t -> aux (i - 1) t
  in
  aux n lst

let solve (delta_x, delta_y) = function
| [] -> 0
| (s :: _) as lst -> 
  let nx = next_x (String.length s) delta_x
  and nl l = match lst_drop_opt (delta_y - 1) l with
  | None -> []
  | Some t -> t
  in
  let rec aux acc x = function
  | [] -> acc
  | h :: t ->
    (string_get_opt h x)
    |> Option.map (fun v -> if (Char.escaped v) = "#" then acc +1 else acc)
    |> fun opt -> aux (Option.value opt ~default: acc) (nx x) (nl t)
  in
  aux 0 0 lst

let solve_q1 = solve (3, 1)
let solve_q2 lst =
  [
    (1, 1)
  ; (3, 1)
  ; (5, 1)
  ; (7, 1)
  ; (1, 2)
  ]
  |> List.map (fun x -> solve x lst)
  |> List.fold_left (fun acc x -> acc * x) 1

let print_answer () =
  print_newline ();
  print_string "D3 Test for Q1: ";
  D3_input.test_input |> solve_q1 |> print_int; (* 7 *)
  print_newline ();
  print_string "D3 Q1: ";
  D3_input.input1 |> solve_q1 |> print_int; (* 198 *)
  print_newline ();
  print_string "D3 Test for Q2: ";
  D3_input.test_input |> solve_q2 |> print_int; (* 336 *)
  print_newline ();
  print_string "D3 Q2: ";
  D3_input.input1 |> solve_q2 |> print_int; (* 5140884672 *)
  print_newline ();