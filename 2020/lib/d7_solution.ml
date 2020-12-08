(* TODO: Use MAP to improve performance *)

type contents = {
  amounts: int
; color: string
}

module Option = struct
  include Option

  let safe_get a = function
  | None -> a
  | Some v -> v

  let (let+) x f = map f x
  let (let*) x f = bind x f
end

let safe_int_of_string str =
  try str |> int_of_string |> Option.some
  with _ -> None
let parse_line str =
  let rec aux acc = function
  | h :: c1 :: c2 :: t when h |> safe_int_of_string |> Option.is_some ->
    let amounts = int_of_string h in
    aux ({ amounts = amounts; color = String.concat " " [c1; c2] } :: acc) t
  | [] -> acc
  | h :: t -> aux acc t
  in
  match String.split_on_char ' ' str with
  | c1 :: c2 :: t -> Some (String.concat " " [c1; c2], aux [] t)
  | _ -> None

let rec find_target dict target contents =
  contents |>
  List.find_opt (fun content -> 
    let res = content.color = target in
    if res then res
    else
      match List.assoc_opt content.color dict with
      | None -> false
      | Some cs -> find_target dict target cs
  )
  |> Option.is_some

let rec calc multiplier dict contents =
  if List.length contents = 0 then [multiplier]
  else 
    multiplier :: List.map (fun c ->
      match List.assoc_opt c.color dict with
      | None -> multiplier
      | Some cs ->
        (calc c.amounts dict cs)
        |> List.fold_left (fun acc x -> (x * multiplier) + acc) 0
    ) contents

let solve_q1 target lst =
  let dict = lst
  |> List.map parse_line
  |> List.fold_left (fun acc -> function
  | None -> acc
  | Some v -> v :: acc
  ) [] in
  List.filter (fun (_, cs) -> find_target dict target cs) dict
  |> List.length

let solve_q2 target lst =
  let dict = lst
  |> List.map parse_line
  |> List.fold_left (fun acc -> function
  | None -> acc
  | Some v -> v :: acc
  ) [] in
  match List.assoc_opt target dict with
  | None -> 0
  | Some cs -> (calc 1 dict cs |> List.fold_left (fun acc x -> acc + x) 0) - 1

let print_answer () =
  print_newline ();
  print_string "D7 Test for Q1: ";
  D7_input.test_input |> solve_q1 "shiny gold" |> print_int; (* 4 *)
  print_newline ();
  print_string "D7 Q1: ";
  D7_input.input1 |> solve_q1 "shiny gold" |> print_int; (* 242 *)
  print_newline ();
  print_string "D7 Test for Q2: ";
  D7_input.test_input2 |> solve_q2 "shiny gold" |> print_int; (* 126 *)
  print_newline ();
  print_string "D7 Q2: ";
  D7_input.input1 |> solve_q2 "shiny gold" |> print_int; (* 176035 *)
  print_newline ();
