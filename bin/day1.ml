open Stdio
open Base

let file = "day1.txt"
let data = In_channel.read_lines file

(* return two sorted list of ints *)
let parse_data data =
  let left, right =
    List.fold_left data ~init:([], []) ~f:(fun (left, right) line ->
      let parsed_line =
        String.split line ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s))
      in
      match parsed_line with
      | [ l; r ] -> Int.of_string l :: left, Int.of_string r :: right
      | _ -> failwith "Invalid input")
  in
  left, right
;;

(* Part 1*)
let () =
  let sorted_left, sorted_right =
    match parse_data data with
    | left, right ->
      List.sort left ~compare:Int.compare, List.sort right ~compare:Int.compare
  in
  let result = List.map2 sorted_left sorted_right ~f:(fun l r -> abs (l - r)) in
  match result with
  | Ok result -> List.fold_left result ~init:0 ~f:( + ) |> printf "Part 1: %d\n"
  | Unequal_lengths -> failwith "Invalid input"
;;

(* Part 2*)
let () =
  let left, right = parse_data data in
  let right_freqs =
    List.fold_left
      right
      ~init:(Map.empty (module Int))
      ~f:(fun acc r ->
        Map.update acc r ~f:(function
          | None -> 1
          | Some v -> v + 1))
  in
  let result =
    List.fold_left left ~init:0 ~f:(fun acc l ->
      match Map.find right_freqs l with
      | Some v -> acc + (v * l)
      | None -> acc)
  in
  printf "Part 2: %d\n" result
;;
