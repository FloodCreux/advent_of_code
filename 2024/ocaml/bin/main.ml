let () = 
  Printf.printf "Day 1: ";
  print_newline();
  Printf.printf "\tPart 1: ";
  Day1.part_1 (open_in "day1.part1.txt");
  Printf.printf "\tPart 2: ";
  Day1.part_2 (open_in "day1.part2.txt");
  print_newline();
  Printf.printf "Day 2: ";
  print_newline();
  Printf.printf "\tPart 1: ";
  Day2.part_1 (open_in "day2.part1.txt");
  print_newline()
