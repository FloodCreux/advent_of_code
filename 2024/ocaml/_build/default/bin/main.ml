let day_1_1 = open_in "day1.part1.txt"
let day_1_2 = open_in "day1.part2.txt"
let () = 
  Printf.printf "Day 1: ";
  print_newline();
  Printf.printf "\tPart 1: ";
  Day1.part_1 day_1_1;
  Printf.printf "\tPart 2: ";
  Day1.part_2 day_1_2;
  print_newline()
