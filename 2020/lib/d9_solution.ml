let invalid target lst =
  let rec aux = function
  | [] -> true
  | h :: t -> 
    match List.find_opt (fun x -> (h + x) = target) t with
    | Some _ -> false
    | None -> aux t
  in
  lst |> aux

let get_or_minus1 opt = Option.value opt ~default: (-1)

let solve_q1 size arr =
  let arr_len = Array.length arr in
  let rec aux n =
    let target_idx = n + size in
    if target_idx >= arr_len then None
    else
      let target = Array.get arr target_idx
      and sub_arr = Array.sub arr n size in
      if sub_arr |> Array.to_list |> invalid target then Some target
      else aux (n + 1)
  in
  aux 0

let solve_q2 size lst =
  let rec find_min_max target acc min max = function
  | [] -> None
  | h :: t when h >= target -> None
  | h :: t ->
    let new_acc = acc + h in
    if new_acc = target then Some (min + max)
    else if new_acc > target then None
    else if h < min then find_min_max target new_acc h max t
    else if h > max then find_min_max target new_acc min h t
    else find_min_max target new_acc min max t
  in
  let rec aux target = function
  | [] -> None
  | (h :: t) as lst -> 
    match find_min_max target 0 Int.max_int Int.min_int lst with
    | None -> aux target t
    | v -> v
  in
  match lst |> Array.of_list |> solve_q1 size with
  | None -> -1
  | Some target -> lst |> aux target |> get_or_minus1

let print_answer () =
  print_newline ();
  print_string "D9 Test for Q1: ";
  D9_input.test_input |> List.map int_of_string |> Array.of_list |> solve_q1 5 |> get_or_minus1 |> print_int; (* 127 *)
  print_newline ();
  print_string "D9 Q1: ";
  D9_input.input1 |> List.map int_of_string |> Array.of_list |> solve_q1 25 |> get_or_minus1 |> print_int; (* 756008079 *)
  print_newline ();
  print_string "D9 Test for Q2: ";
  D9_input.test_input |> List.map int_of_string |> solve_q2 5 |> print_int; (* 62 *)
  print_newline ();
  print_string "D9 Q2: ";
  D9_input.input1 |> List.map int_of_string |> solve_q2 25 |> print_int; (* 93727241 *)