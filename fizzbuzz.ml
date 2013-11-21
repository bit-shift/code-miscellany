open Core.Std;;

let count n0 nN =
  let rec count_loop n l =
    if n < n0 then l else count_loop (n-1) (n::l)
  in count_loop nN []
;;

  let rec revFizzBuzz n = match n with
  | 0 -> []
  | n -> let fizz = if (n % 3) = 0 then "Fizz" else ""
         and buzz = if (n % 5) = 0 then "Buzz" else "" in
         let fb = (fizz ^ buzz) in
         let output = (if fb = "" then Int.to_string n else fb) in
         output :: (revFizzBuzz (n - 1))
  ;;

let fizzBuzz n =
  let rec fizzBuzzLoop n acc = match n with
  | 0 -> acc
  | n -> let fizz = if (n % 3) = 0 then "Fizz" else ""
         and buzz = if (n % 5) = 0 then "Buzz" else "" in
         let fb = (fizz ^ buzz) in
         let output = (if fb = "" then Int.to_string n else fb) in
         fizzBuzzLoop (n - 1) (output :: acc)
  in
  fizzBuzzLoop n []
;;

let () =
  let args = Array.to_list (Array.slice Sys.argv 1 0) in
  let iterCount = match args with
  | [] -> 100
  | [n] -> Int.of_string n
  | _ -> failwith "Too many arguments, none or one expected."
  in
  if iterCount > 0 then List.iter ~f:print_endline (fizzBuzz iterCount)
  else failwith "Target number must be greater than 0."
;;
