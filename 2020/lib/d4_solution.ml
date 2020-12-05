let str_to_tup str =
  match String.split_on_char ':' str with
  | k :: v :: [] -> [(k, v)]
  | _ -> []

let process_data raw_data =
  String.split_on_char '\n' (String.concat "\n" [raw_data; ""])
  |> List.fold_left (fun (acc, temp_acc) l -> 
    if l = "" 
    then ((String.concat " " temp_acc) :: acc, [])
    else (acc, l :: temp_acc)
  ) ([], []) 
  |> fun (lst, _) -> List.map (fun l ->
    l
    |> String.split_on_char ' '
    |> List.map str_to_tup
    |> List.flatten
  ) lst

let check_q1 lst =
  let checklist = [
    "byr"
  ; "iyr"
  ; "eyr"
  ; "hgt"
  ; "hcl"
  ; "ecl"
  ; "pid"
  ] in
  checklist
  |> List.map (fun k -> List.mem_assoc k lst)
  |> List.for_all (fun x -> x)

let check_q2 (lst: (string * string) list) =
  let eye_color_lst = [
    "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"
  ]
  and valid_hair_code_lst = [
    'a'; 'b'; 'c'; 'd'; 'e'; 'f'
  ] (* cannot load Str module. Hardcoding *)
  and check_range min max v =
    try 
      let parsed = int_of_string v in 
      parsed >= min && parsed <= max 
    with _ -> false
  in
  let check_bth = check_range 1920 2002
  and check_iss = check_range 2010 2020
  and check_exp = check_range 2020 2030
  and check_eye color = (List.find_opt (fun x -> x = color) eye_color_lst)
  |> Option.map (fun x -> true)
  |> fun x -> Option.value x ~default: false
  and check_pid pid =
    let length_check_res = (String.length pid) = 9 in
    let int_parse_result =
      try int_of_string pid |> fun _ -> true
      with _ -> false
    in
    length_check_res && int_parse_result
  and check_hair color =
    let rec aux = function
    | [] -> true
    | h :: t ->
      try int_of_char h |> fun _ -> aux t
      with _ -> match List.find_opt (fun x -> x = h) valid_hair_code_lst with
      | None -> false
      | Some _ -> aux t
    in
    match color |> String.to_seq |> List.of_seq with
    | h :: t -> if h = '#' then aux t else false
    | [] -> false
  and check_height str =
    let check_cm  = check_range 150 193
    and check_in  = check_range 59 76
    and aux f lst =
      try List.rev lst |> List.to_seq |> String.of_seq |> f
      with _ -> false
    in
    let reversed =
      String.to_seq str
      |> Seq.fold_left (fun acc x -> x :: acc) [] in
    match reversed with
    | 'n' :: 'i' :: t -> aux check_in t
    | 'm' :: 'c' :: t -> aux check_cm t
    | _ -> false
  in
  let checklist = [
    ("byr", check_bth)
  ; ("iyr", check_iss)
  ; ("eyr", check_exp)
  ; ("hgt", check_height)
  ; ("hcl", check_hair)
  ; ("ecl", check_eye)
  ; ("pid", check_pid)
  ] in
  let aux f = function
  | None -> false
  | Some v -> f v in
  checklist
  |> List.map (fun (k, validate) -> List.assoc_opt k lst |> aux validate)
  |> List.for_all (fun x -> x)

let solve solver lst =
  lst
  |> List.filter solver
  |> List.length

let print_answer () =
  print_newline ();
  print_string "D4 Test for Q1: ";
  D4_input.test_input |> process_data |> solve check_q1 |> print_int; (* 2 *)
  print_newline ();
  print_string "D4 Q1: ";
  D4_input.input1 |> process_data |> solve check_q1 |> print_int; (* 242 *)
  print_newline ();
  print_string "D4 Test for Q2: ";
  D4_input.test_input2 |> process_data |> solve check_q2 |> print_int; (* 4 *)
  print_newline ();
  print_string "D4 Q2: ";
  D4_input.input1 |> process_data |> solve check_q2 |> print_int; (* 186 *)
  print_newline ();