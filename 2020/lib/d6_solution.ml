
module CharOrdered = struct
  type t = char
  let compare = compare
end
module CharSet = Set.Make(CharOrdered)
let group_data lst =
  let flatten_str_lst l =
    let char_lst_lst = l
    |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
    in
    (char_lst_lst
    |> List.flatten
    |> CharSet.of_list, char_lst_lst)
  in
  let rec aux acc temp = function
  | [] -> temp :: acc
  | "" :: t -> aux (temp :: acc) [] t
  | h :: t -> aux acc (h :: temp) t
  in
  aux [] [] lst |> List.map flatten_str_lst

let solve_q1 set_lst =
  set_lst
  |> List.map (fun (s, _) -> s |> CharSet.cardinal)
  |> List.fold_left (fun acc x -> acc + x) 0

let solve_q2 set_lst =
  let get_intersection lst =
    match List.map (fun l -> l |> List.to_seq |> CharSet.of_seq) lst with
    | [] -> CharSet.empty
    | h :: t -> List.fold_left (fun acc x -> CharSet.inter acc x) h t
  in
  set_lst
  |> List.map (fun (_, l) -> get_intersection l |> CharSet.cardinal)
  |> List.fold_left (fun acc x -> acc + x) 0

let print_answer () =
  print_newline ();
  print_string "D6 Test for Q1: ";
  D6_input.test_input |> group_data |> solve_q1 |> print_int (* 11 *);
  print_newline ();
  print_string "D6 Q1: ";
  D6_input.input1 |> group_data |> solve_q1 |> print_int (* 7027 *);
  print_newline ();
  print_string "D6 Test for Q2: ";
  D6_input.test_input |> group_data |> solve_q2 |> print_int (* 6 *);
  print_newline ();
  print_string "D6 Q2: ";
  D6_input.input1 |> group_data |> solve_q2 |> print_int (* 259 *);
  print_newline ();