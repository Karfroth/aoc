let validate_with_count min max c passcode =
  let count = passcode
  |> String.to_seq
  |> (Seq.map (fun x -> (Char.escaped x) = c))
  |> Seq.fold_left (fun acc x -> if x then acc + 1 else acc) 0 in
  count >= min && count <= max

let string_get_opt str i =
  try Some (String.get str i)
  with _ -> None
let validate_with_position true_idx false_idx c passcode =
  let char_at_true_idx = string_get_opt passcode true_idx
  and char_at_false_idx = string_get_opt passcode false_idx
  and check_func = Option.map (fun x -> if (Char.escaped x) = c then 1 else 0) in
  
  match (char_at_true_idx |> check_func, char_at_false_idx |> check_func) with
  | ((Some a), (Some b)) -> (a + b) = 1
  | _ -> false

let check_passcode validate_func policy passcode =
  let aux rangeStr c =
    match List.map int_of_string (String.split_on_char '-' rangeStr) with
    | min :: max :: [] -> validate_func min max c passcode
    | _ -> false
  in
  match String.split_on_char ' ' policy with
  | rangeStr :: c :: [] -> aux rangeStr c
  | _ -> false
let validate_policy_passcode_set validate_func str =
  match String.split_on_char ':' str with
  | policy :: passcode :: [] -> check_passcode validate_func policy passcode
  | _ -> false

let count_valid_policy_passcode_set validate_func lst =
  lst
  |> List.map (validate_policy_passcode_set validate_func)
  |> List.fold_left (fun acc x -> if x then acc + 1 else acc) 0

  let print_answer () =
    print_newline ();
    print_string "D2 Test for Q1: ";
    D2_input.test_input |> count_valid_policy_passcode_set validate_with_count |> print_int; (* 2 *)
    print_newline ();
    print_string "D2 Q1: ";
    D2_input.input1 |> count_valid_policy_passcode_set validate_with_count |> print_int; (* 483 *)
    print_newline ();
    print_string "D2 Test for Q2: ";
    D2_input.test_input |> count_valid_policy_passcode_set validate_with_position |> print_int; (* 1 *)
    print_newline ();
    print_string "D2 Q2: ";
    D2_input.input1 |> count_valid_policy_passcode_set validate_with_position |> print_int; (* 482 *)