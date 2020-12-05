let print_lst printer lst =
  print_newline();
  List.iter (fun x -> printer x; print_string "; ") lst

let split_by_idx i lst =
  let (l1, l2) = 
    lst
    |> List.mapi( fun n x -> (n, x))
    |> List.partition (fun (n, x) -> n < i)
  in
  let aux l = List.map (fun (n, x) -> x) l in
  (aux l1, aux l2)

let preprocess_input str =
  let index = (String.length str) - 3 in
  str
  |> String.to_seq
  |> List.of_seq
  |> split_by_idx index

let range_list min max =
  let rec aux n acc =
    if n = min then min :: acc
    else aux (n - 1) (n :: acc)
  in
  aux (max - 1) []

let hd_opt lst =
  if List.length lst > 0 then Some (List.hd lst)
  else None
let tl_opt lst =
  if List.length lst > 0 then Some (List.tl lst)
  else None

let tictac f_command b_command lst commands =
  let rec aux lst commands =
    let length = List.length lst in
    if length = 1 then hd_opt lst
    else 
      let (f, b) = split_by_idx ((List.length lst) / 2) lst in
      match commands with
      | command :: t when command = f_command -> aux f t
      | command :: t when command = b_command -> aux b t
      | _ -> None
  in
  aux lst commands

module Option = struct
  include Option

  let safe_get a = function
  | None -> a
  | Some v -> v

  let (let+) x f = map f x
  let (let*) x f = bind x f
end

let calc_row_col_opt (r_min, r_max) (c_min, c_max) command =
  let (row_commands, col_commands) = preprocess_input command in
  let open Option in
  let* row = tictac 'F' 'B' (range_list r_min r_max) row_commands in
  let+ col = tictac 'L' 'R' (range_list c_min c_max) col_commands in
  (row, col)

let calc row col = (row * 8) + col

let calc_seat_id row_range col_range command = 
  let seat_id_opt =
    let open Option in
    let+ (row, col) = calc_row_col_opt row_range col_range command in
    calc row col
  in
  Option.value seat_id_opt ~default: (-1)

let find_max_seat_id row_range col_range lst =
  lst
  |> List.map (calc_seat_id row_range col_range)
  |> List.fast_sort (fun x y -> y - x)
  |> hd_opt

let find_missing_seats row_range col_range lst =
  let seat_ids = List.map (calc_seat_id row_range col_range) lst |> List.sort (fun x y -> x - y) in
  print_lst print_int seat_ids;
  let open Option in
  let+ tl = tl_opt seat_ids in
  let (toAdd, _) = List.combine seat_ids (tl @ [Int.zero])
  |> List.find (fun (x, y) -> y - x <> 1) in
  toAdd + 1

let print_answer () =
  print_newline ();
  print_string "D5 Test for Q1: ";
  D5_input.test_input |> find_max_seat_id (0, 128) (0, 8) |> Option.safe_get (-1) |> print_int; (* 820 *)
  print_newline ();
  print_string "D5 Q1: ";
  D5_input.input1 |> find_max_seat_id (0, 128) (0, 8) |> Option.safe_get (-1) |> print_int; (* 888 *)
  print_newline ();
  print_string "D5 Q2: ";
  D5_input.input1 |> find_missing_seats (0, 128) (0, 8) |> Option.safe_get (-1) |> print_int; (* 522 *)
