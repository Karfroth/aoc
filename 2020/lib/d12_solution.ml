type command =
| N of int
| S of int
| E of int
| W of int
| L of int
| R of int
| F of int

type direction =
| North
| South
| West
| East

type point = {
  x: int
; y: int
}
type waypoint = {
  direction: direction
}
type state = {
  p: point
; a: int
}

module IntMap = Map.Make(Int)

exception Unknown_Angle

let print_state state =
  print_string "x: ";
  print_int state.p.x;
  print_string "; ";
  print_string "y: ";
  print_int state.p.y;
  print_string "; ";
  print_string "a: ";
  print_int state.a;
  print_string "; ";
  print_newline ()

let angle_to_direction = function
| 0   -> East
| 90  -> North
| 180 -> West
| 270 -> South
| rest  -> print_int rest; print_newline (); raise Unknown_Angle

let id x = x
let build_command_opt f i_char_seq = i_char_seq |> String.of_seq |> int_of_string_opt |> Option.map f
let parse str =
  match String.to_seq str () with
  | Seq.Cons('N', t) -> build_command_opt (fun x -> N x) t
  | Seq.Cons('S', t) -> build_command_opt (fun x -> S x) t
  | Seq.Cons('E', t) -> build_command_opt (fun x -> E x) t
  | Seq.Cons('W', t) -> build_command_opt (fun x -> W x) t
  | Seq.Cons('L', t) -> build_command_opt (fun x -> L x) t
  | Seq.Cons('R', t) -> build_command_opt (fun x -> R x) t
  | Seq.Cons('F', t) -> build_command_opt (fun x -> F x) t
  | _ -> None

let preprocess = List.filter_map parse

let update_angle a v = 
  let new_angle = (a + v) mod 360 in
  if new_angle < 0 then new_angle + 360 else new_angle

let rec apply_command_q1 state = function
| N v -> { state with p = { state.p with y = state.p.y + v } }
| S v -> { state with p = { state.p with y = state.p.y - v } }
| E v -> { state with p = { state.p with x = state.p.x + v } }
| W v -> { state with p = { state.p with x = state.p.x - v } }
| L v -> { state with a = update_angle state.a v }
| R v -> { state with a = update_angle state.a (-1 * v) }
| F v -> 
  let new_command = match angle_to_direction state.a with
  | North -> N v
  | South -> S v
  | East -> E v
  | West -> W v
  in
  apply_command_q1 state new_command

let rec apply_command_q2 (sp, wp) = function
| N v -> (sp, { wp with y = wp.y + v })
| S v -> (sp, { wp with y = wp.y - v })
| E v -> (sp, { wp with x = wp.x + v })
| W v -> (sp, { wp with x = wp.x - v })
| L v ->
  let new_wp = match v with
  | 90  -> { x = -wp.y; y =  wp.x }
  | 180 -> { x = -wp.x; y = -wp.y }
  | 270 -> { x =  wp.y; y = -wp.x }
  | _   -> wp
  in
  (sp, new_wp)
| R v ->
  let new_wp = match v with
  | 90  -> { x =  wp.y; y = -wp.x }
  | 180 -> { x = -wp.x; y = -wp.y }
  | 270 -> { x = -wp.y; y =  wp.x }
  | _   -> wp
  in
  (sp, new_wp)
| F v -> ({ x = (wp.x * v) + sp.x; y = (wp.y * v) + sp.y }, wp)

let solve_q1 lst =
  let final_state = lst
  |> preprocess
  |> List.fold_left apply_command_q1 { p = { x = 0; y = 0 }; a = 0 } in
  (Int.abs final_state.p.x) + (Int.abs final_state.p.y)

let solve_q2 lst =
  let final_state = lst
  |> preprocess
  |> List.fold_left apply_command_q2 ({x = 0; y = 0}, {x = 10; y = 1})
  |> fst in
  (Int.abs final_state.x) + (Int.abs final_state.y)

let print_answer () =
  print_newline ();
  print_string "D12 Test1 for Q1: ";
  D12_input.test_input |> solve_q1 |> print_int; (* 25 *)
  print_newline ();
  print_string "D12 Q1: ";
  D12_input.input1 |> solve_q1 |> print_int; (* 1010 *)
  print_newline ();
  print_string "D12 Test1 for Q2: ";
  D12_input.test_input |> solve_q2 |> print_int; (* 286 *)
  print_newline ();
  print_string "D12 Q2: ";
  D12_input.input1 |> solve_q2 |> print_int; (* 52742 *)