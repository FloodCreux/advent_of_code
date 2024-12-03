(* mul\(\d+,\d+\) *)
let split_lines lines = 
  List.map (fun input ->
    let re = Str.regexp "mul\\(\\(\\d+\\),\\(\\d+\\)\\)" in
    let rec collect acc pos =
      try
        let _ = Str.search_forward re input pos in
        let first_num = int_of_string (Str.matched_group 1 input) in
        let second_num = int_of_string (Str.matched_group 2 input) in
        Printf.printf "%d %d \n" first_num second_num;
        collect ((first_num * second_num) :: acc) (Str.match_end ())
      with Not_found -> List.rev acc
    in
    let result = collect [] 0 in  
    Printf.printf "%s \n" input;
    List.fold_left (+) 0 result
  ) lines

let part_1 file_channel = 
  let all_lines = Tools.input_lines file_channel in
  let mul_lists = split_lines all_lines in
  List.iter (fun i -> Printf.printf "%d \n" i) mul_lists
  
