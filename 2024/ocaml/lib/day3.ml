(* mul\(\d+,\d+\) *)
let split_lines lines = 
  List.map (fun input ->
    let re = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
    let rec collect acc pos =
      try
        let _ = Str.search_forward re input pos in
        let first_num = int_of_string (Str.matched_group 1 input) in
        let second_num = int_of_string (Str.matched_group 2 input) in
        collect ((first_num * second_num) :: acc) (Str.match_end ())
      with Not_found -> List.rev acc
    in
    let result = collect [] 0 in  
    List.fold_left (+) 0 result
  ) lines

let part_1 file_channel = 
  let all_lines = Tools.input_lines file_channel in
  let mul_lists = split_lines all_lines in
  let result = List.fold_left (+) 0 mul_lists in
  Printf.printf "%d \n" result  

let strip_dont_dos lines =
  List.map (fun line ->
    let dont_re = Str.regexp "don\\'t().*do()" in
    let result = Str.global_replace dont_re "" line in
    Printf.printf "original: %s\nstripped: %s\n\n" line result;
    result
  ) lines

let part_2 file_channel =
  let all_lines = Tools.input_lines file_channel in
  let single_result = String.concat "" all_lines in
  let cleansed = strip_dont_dos [single_result] in
  let mul_lists = split_lines cleansed in
  let result = List.fold_left (+) 0 mul_lists in
  Printf.printf "%d \n" result
