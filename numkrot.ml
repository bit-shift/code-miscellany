open Core.Std;;

(* cipher creation function *)
let buildCipher mappings =
  let encodeMappings = List.map ~f:(fun (i, o) -> (Char.uppercase i, Char.uppercase o)) mappings in
  let decodeMappings = List.map ~f:Tuple2.swap encodeMappings
  and transcoder m c =
    match List.Assoc.find m c with
    | Some c' -> c'
    | None    -> match List.Assoc.find m (Char.uppercase c) with
                 | Some c' -> (Char.lowercase c')
                 | None    -> c
  in (transcoder mappings, transcoder decodeMappings)
;;

(* slurping operator *)
let (<@>) filename handler =
  let chan = match filename with
  | "-" -> stdin
  | filename -> In_channel.create filename
  in
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

(* file/process applier *)
let process_file processor filename =
  let lineHandler s = println (String.map ~f:processor s) in
  filename <@> lineHandler
;;

(* produce our numkrot encoder/decoder pair *)
let (numkrot, denumkrot) = buildCipher [('A','U') ; ('B','V') ; ('C','S') ; ('D','T') ; ('E','O') ; ('F','H') ; ('G','K') ; ('H','F') ; ('I','Y') ; ('J','Z') ; ('K','G') ; ('L','R') ; ('M','N') ; ('N','M') ; ('O','E') ; ('P','X') ; ('Q','W') ; ('R','L') ; ('S','C') ; ('T','D') ; ('U','A') ; ('V','B') ; ('W','P') ; ('X','Q') ; ('Y','I') ; ('Z','J')];;

(* and finally pull it all together *)
let () =
  let args = Array.to_list (Array.slice Sys.argv 1 0) in
  match args with
  | []       -> ignore (process_file numkrot "-");
                exit 0
  | ["-d"]   -> ignore (process_file denumkrot "-");
                exit 0
  | "-d"::xs -> ignore (List.map ~f:(process_file denumkrot) xs);
                exit 0
  | xs       -> ignore (List.map ~f:(process_file numkrot) xs);
                exit 0
;;
