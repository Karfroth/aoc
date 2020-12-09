type ops = ACC of int | NOP of int | JMP of int

module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let parse_num str =
  match String.to_seq str () with
  | Seq.Cons('-', num) -> num |> String.of_seq |> int_of_string_opt |> Option.map (fun x -> -1 * x)
  | Seq.Cons('+', num) -> num |> String.of_seq |> int_of_string_opt
  | _ -> None

let to_ops = function
| "acc" :: num :: _ -> num |> parse_num |> Option.map (fun x -> ACC x)
| "nop" :: num :: _ -> num |> parse_num |> Option.map (fun x -> NOP x)
| "jmp" :: num :: _ -> num |> parse_num |> Option.map (fun x -> JMP x)
| _ -> None

let parse lst =
  let lst_with_idx = lst |> List.mapi (fun i x -> (i, x)) in
  let ops_seq_with_idx = Seq.filter_map (fun (i, str) -> 
    str |> String.split_on_char ' ' |> to_ops |> Option.map (fun x -> (i, x))
  ) (List.to_seq lst_with_idx) in
  IntMap.of_seq ops_seq_with_idx

let solve_q1 dict =
  let rec aux acc visit_set current_idx =
    if IntSet.mem current_idx visit_set then acc
    else
      let new_visit_set = IntSet.add current_idx visit_set in
      match IntMap.find_opt current_idx dict with
      | None -> acc
      | Some (ACC num) -> aux (acc + num) new_visit_set (current_idx + 1)
      | Some (JMP num) -> aux acc new_visit_set (current_idx + num)
      | Some (NOP  _ ) -> aux acc new_visit_set (current_idx + 1)
  in
  aux 0 IntSet.empty 0

let search_next_nop_jmp_idx dict idx_set =
  IntMap.find_first_opt (fun i ->
  if IntSet.mem i idx_set then false
  else match IntMap.find_opt i dict with
  | Some (JMP _) | Some (NOP _) -> true
  | _ -> false
  ) dict
  |> Option.map (fun (i, _) -> i)

let solve_q2 dict =
  let rec aux acc current_target_nop_jmp_idx_opt visit_set tried_nop_jmp_idx_set current_idx =
    match current_target_nop_jmp_idx_opt with
    | None -> "Something went wrong"
    | Some current_target_nop_jmp_idx -> 
      let nop num new_visit_set = aux acc current_target_nop_jmp_idx_opt new_visit_set tried_nop_jmp_idx_set (current_idx + 1)
      and jmp num new_visit_set = aux acc current_target_nop_jmp_idx_opt new_visit_set tried_nop_jmp_idx_set (current_idx + num) in
      if IntSet.mem current_idx visit_set
      then aux 0 (search_next_nop_jmp_idx dict tried_nop_jmp_idx_set) IntSet.empty (IntSet.add current_target_nop_jmp_idx tried_nop_jmp_idx_set) 0
      else
        let new_visit_set = IntSet.add current_idx visit_set in
        match IntMap.find_opt current_idx dict with
        | None -> acc |> string_of_int
        | Some (ACC num) -> aux (acc + num) current_target_nop_jmp_idx_opt new_visit_set tried_nop_jmp_idx_set (current_idx + 1)
        | Some (JMP num) -> if current_idx = current_target_nop_jmp_idx then nop num new_visit_set else jmp num new_visit_set
        | Some (NOP num) -> if current_idx = current_target_nop_jmp_idx then jmp num new_visit_set else nop num new_visit_set
  in
  aux 0 (search_next_nop_jmp_idx dict IntSet.empty) IntSet.empty IntSet.empty 0

let print_answer () =
  print_newline ();
  print_string "D8 Test for Q1: ";
  D8_input.test_input |> parse |> solve_q1 |> print_int; (* 5 *)
  print_newline ();
  print_string "D8 Q1: ";
  D8_input.input1 |> parse |> solve_q1 |> print_int; (* 1553 *)
  print_newline ();
  print_string "D8 Test for Q2: ";
  D8_input.test_input |> parse |> solve_q2 |> print_string; (* 8 *)
  print_newline ();
  print_string "D8 Q2: ";
  D8_input.input1 |> parse |> solve_q2 |> print_string; (* 1877 *)



  