type state = Empty | Occupied | Floor
exception Unknown_State_Char

let print_state = function
| Empty -> print_string "L"
| Occupied -> print_string "#"
| Floor -> print_string "."

let state_of_char = function
| '#' -> Occupied
| 'L' -> Empty
| '.' -> Floor
| _ -> raise Unknown_State_Char

let preprocess arr =
  Array.map (fun x -> 
    x
    |> String.to_seq
    |> Array.of_seq
    |> Array.map state_of_char
    ) arr

let get_from_2d_arr r_i c_i arr_arr = 
  let row = Array.get arr_arr r_i in
  Array.get row c_i

let calc_occupied arr_arr =
  arr_arr |> List.filter (fun x -> x = Occupied) |> List.length

let safe_get r_i c_i arr =
  try get_from_2d_arr r_i c_i arr |> Option.some
  with _ -> None

let collect_opt lst =
  let rec aux acc = function
  | [] -> acc
  | (Some v) :: t -> aux (v :: acc) t
  | None :: t -> aux acc t
  in
  aux [] lst

let search_direction f point arr_arr =
  [
    (-1, -1); (-1, 0); (-1, 1);
    ( 0, -1);          ( 0, 1);
    ( 1, -1); ( 1, 0); ( 1, 1)
  ]
  |> f point arr_arr
  |> collect_opt

let adjacent (r_i, c_i) arr_arr =
  List.map (fun (r, c) -> safe_get (r + r_i) (c + c_i) arr_arr)

let sight (r_i, c_i) arr_arr =
  let rec search delta_r delta_c r c =
    match safe_get r c arr_arr with
    | None -> None
    | Some Floor -> search delta_r delta_c (r + delta_r) (c + delta_c)
    | Some v -> Some v
  in
  List.map (fun (delta_r, delta_c) -> search delta_r delta_c (r_i + delta_r) (c_i + delta_c))

let next_seat_state tolerance f point arr_arr =
  let current_seat_state = get_from_2d_arr (fst point) (snd point) arr_arr
  and occupied_seats_n = arr_arr |> f point |> calc_occupied in
  match (current_seat_state, occupied_seats_n) with
  | (Empty, 0) -> Occupied
  | (Occupied, n) when n >= tolerance -> Empty
  | (seat_state, _) -> seat_state

let next_state f arr_arr =
  arr_arr |> Array.mapi (fun r_i col_arr -> Array.mapi (fun c_i s -> f (r_i, c_i) arr_arr) col_arr)

let compare_arrays arr1 arr2 =
  let aux r_i col_arr =
    let bool_arr = Array.mapi (fun c_i s -> s = (get_from_2d_arr r_i c_i arr2)) col_arr in
    Array.fold_left (fun acc x -> acc && x) true bool_arr
  in
  arr1 |> Array.mapi aux |> Array.fold_left (fun acc x -> acc && x) true

let compute tolerance f arr_arr =
  let rec aux a =
    let find = search_direction f in
    let next = next_state (next_seat_state tolerance find) a in
    if compare_arrays next a then next
    else aux next
  in
  arr_arr
  |> aux
  |> Array.map (fun x -> Array.to_list x)
  |> Array.to_list
  |> List.flatten
  |> calc_occupied

let solve_q1 arr_arr = compute 4 adjacent arr_arr

let solve_q2 arr_arr = compute 5 sight arr_arr

let print_answer () =
  print_newline ();
  print_string "D11 Test1 for Q1: ";
  D11_input.test_input |> preprocess |> solve_q1 |> print_int; (* 37 *)
  print_newline ();
  print_string "D11 Q1: ";
  D11_input.input1 |> preprocess |> solve_q1 |> print_int; (* 2243 *)
  print_newline ();
  print_string "D11 Test1 for Q2: ";
  D11_input.test_input |> preprocess |> solve_q2 |> print_int; (* 26 *)
  print_newline ();
  print_string "D11 Q2: ";
  D11_input.input1 |> preprocess |> solve_q2 |> print_int; (* 2027 *)
  print_newline ();