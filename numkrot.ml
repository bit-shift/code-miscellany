open Core.Std;;

(* pipeline operator *)
let (>>) f g x = g (f x);;

(* slurping operator *)
let (<@>) filename handler =
  let chan = In_channel.create filename in
  let rec reader_loop acc =
    let line = try Some (input_line chan)
               with End_of_file -> None in
    match line with
    | Some line -> reader_loop ((handler line) :: acc)
    | None -> List.rev acc
  in
  let out = reader_loop [] in
  In_channel.close chan;
  out
;;

(* convenience function for printing a line *)
let println s = print_string (s ^ "\n");;

(* string encoder *)
let numkrot s =
  let numkrot_char c =
    match c with
    | 'A' -> 'U' | 'a' -> 'u'
    | 'B' -> 'V' | 'b' -> 'v'
    | 'C' -> 'S' | 'c' -> 's'
    | 'D' -> 'T' | 'd' -> 't'
    | 'E' -> 'O' | 'e' -> 'o'
    | 'F' -> 'H' | 'f' -> 'h'
    | 'G' -> 'K' | 'g' -> 'k'
    | 'H' -> 'F' | 'h' -> 'f'
    | 'I' -> 'Y' | 'i' -> 'y'
    | 'J' -> 'Z' | 'j' -> 'z'
    | 'K' -> 'G' | 'k' -> 'g'
    | 'L' -> 'R' | 'l' -> 'r'
    | 'M' -> 'N' | 'm' -> 'n'
    | 'N' -> 'M' | 'n' -> 'm'
    | 'O' -> 'E' | 'o' -> 'e'
    | 'P' -> 'X' | 'p' -> 'x'
    | 'Q' -> 'W' | 'q' -> 'w'
    | 'R' -> 'L' | 'r' -> 'l'
    | 'S' -> 'C' | 's' -> 'c'
    | 'T' -> 'D' | 't' -> 'd'
    | 'U' -> 'A' | 'u' -> 'a'
    | 'V' -> 'B' | 'v' -> 'b'
    | 'W' -> 'P' | 'w' -> 'p'
    | 'X' -> 'Q' | 'x' -> 'q'
    | 'Y' -> 'I' | 'y' -> 'i'
    | 'Z' -> 'J' | 'z' -> 'j'
    | c -> c
  in
  String.map ~f:numkrot_char s
;;

(* string decoder *)
let denumkrot s =
  let denumkrot_char c =
    match c with
    | 'A' -> 'U' | 'a' -> 'u'
    | 'B' -> 'V' | 'b' -> 'v'
    | 'C' -> 'S' | 'c' -> 's'
    | 'D' -> 'T' | 'd' -> 't'
    | 'E' -> 'O' | 'e' -> 'o'
    | 'F' -> 'H' | 'f' -> 'h'
    | 'G' -> 'K' | 'g' -> 'k'
    | 'H' -> 'F' | 'h' -> 'f'
    | 'I' -> 'Y' | 'i' -> 'y'
    | 'J' -> 'Z' | 'j' -> 'z'
    | 'K' -> 'G' | 'k' -> 'g'
    | 'L' -> 'R' | 'l' -> 'r'
    | 'M' -> 'N' | 'm' -> 'n'
    | 'N' -> 'M' | 'n' -> 'm'
    | 'O' -> 'E' | 'o' -> 'e'
    | 'P' -> 'W' | 'p' -> 'w'
    | 'Q' -> 'X' | 'q' -> 'x'
    | 'R' -> 'L' | 'r' -> 'l'
    | 'S' -> 'C' | 's' -> 'c'
    | 'T' -> 'D' | 't' -> 'd'
    | 'U' -> 'A' | 'u' -> 'a'
    | 'V' -> 'B' | 'v' -> 'b'
    | 'W' -> 'Q' | 'w' -> 'q'
    | 'X' -> 'P' | 'x' -> 'p'
    | 'Y' -> 'I' | 'y' -> 'i'
    | 'Z' -> 'J' | 'z' -> 'j'
    | c -> c
  in
  String.map ~f:denumkrot_char s
;;

(* file encoder *)
let encode_file filename =
  let pipeline = (numkrot >> println) in
  filename <@> pipeline
;;

(* file decoder *)
let decode_file filename =
  let pipeline = (denumkrot >> println) in
  filename <@> pipeline
;;


let () =
  let args = Array.to_list (Array.slice Sys.argv 1 0) in
  match args with
  | []       -> exit 1
  | "-d"::xs -> ignore (List.map ~f:decode_file xs);
                   exit 0
  | xs       -> ignore (List.map ~f:encode_file xs);
                   exit 0
;;
