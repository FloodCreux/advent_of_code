module IntMap = Tools.IntMap

let equal_int x y = x = y
let abs_diff x y = Int.abs (x - y)
let int_of_bool b = if b then 1 else 0

let rec list_compare lst = 
  match lst with
  | [] -> true
  | x :: xs -> if not x then false else list_compare xs

let split_lines lines =
  List.map (fun line ->
    let levels = Tools.split_line line in
    List.map (fun x -> int_of_string x) levels
  ) lines

let in_order report = 
  let asc = List.sort compare report in
  let desc = List.rev asc in
  List.equal equal_int report asc || List.equal equal_int report desc

let rec compare_levels levels = 
  match levels with
  | [] | [_] -> [] 
  | x :: y :: tail ->
    let abs = abs_diff x y in
    let comp = abs > 0 && abs < 4 in
    comp :: compare_levels (y :: tail)

let close_levels report =
  let comparisons = compare_levels report in
  list_compare comparisons
  

let part_1 file_channel = 
  let all_lines = Tools.input_lines file_channel in
  let reports = split_lines all_lines in
  let scanned = List.map (fun level -> 
    in_order level && close_levels level
  ) reports in
  let result = List.fold_left (fun acc x -> 
    acc + (int_of_bool x)
  ) 0 scanned in
  Printf.printf "%d " result

let rec compare_levels_2 retries levels = 
  let ordered = in_order levels in
  match levels with
  | [] | [_] -> []
  | x :: y :: tail ->
    let abs = abs_diff x y in
    let comp = ordered && abs > 0 && abs < 4 in
    if comp || retries > 1 then begin
      comp :: compare_levels_2 retries (y :: tail)
    end 
    else begin
      compare_levels_2 (retries + 1) (x :: tail)
    end

let close_levels_2 levels =
  let comparisons = compare_levels_2 0 levels in
  list_compare comparisons

let part_2 file_channel = 
  let all_lines = Tools.input_lines file_channel in
  let reports = split_lines all_lines in
  let scanned = List.map (fun level -> 
    close_levels_2 level
  ) reports in
  let result = List.fold_left (fun acc x -> 
    acc + (int_of_bool x)
  ) 0 scanned in
  Printf.printf "%d " result;
  print_newline ()
