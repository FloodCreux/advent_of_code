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

let split_line line =
  Str.split (Str.regexp "[ \n\r\x0c\t]+") line
