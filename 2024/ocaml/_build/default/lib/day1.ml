module IntMap = Map.Make(struct
    type t = int
    let compare = compare
end)

let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

let split_lines lines = 
  let split_line line =
    Str.split (Str.regexp "[ \n\r\x0c\t]+") line
  in
  let list1, list2 =
    List.fold_left (fun (lst1, lst2) line ->
      let positions = split_line line in
      (lst1 @ [int_of_string (List.hd positions)], lst2 @ [int_of_string (List.hd (List.tl positions))])
    ) ([], []) lines
  in
  (list1, list2)

let diff_lists lst1 lst2 = 
    List.map2 (fun x y -> if x > y then x - y else y - x) lst1 lst2

let part_1 file_channel =
  let all_lines = input_lines file_channel in
  let array1, array2 = split_lines all_lines in
  let result = List.fold_left (fun acc x -> acc + x) 0 (diff_lists (List.sort compare array1) (List.sort compare array2)) in
  Printf.printf "%d " result;
  print_newline ()

let list_to_map lst = 
  List.fold_left
    (fun acc x ->
      let count = match IntMap.find_opt x acc with
        | Some c -> c + 1
        | None -> 1
      in
      IntMap.add x count acc)
    IntMap.empty
    lst

let total_occurrences lst map = 
  List.fold_left (fun acc x ->
    let mult = match IntMap.find_opt x map with
      | Some v -> v
      | None -> 0
    in
    acc + x * mult
  ) 0 lst
        
let part_2 file_channel =
  let all_lines = input_lines file_channel in
  let array1, array2 = split_lines all_lines in
  let result = total_occurrences array1 (list_to_map array2) in
  Printf.printf "%d " result;
  print_newline ()
