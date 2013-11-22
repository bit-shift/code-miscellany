open Core.Std;;

let rec list_permutations l =
  let list_length = List.length l in
  let rec inner_loop n acc =
    match n = list_length with
    | true  -> acc
    | false -> let front = List.nth l n
               and back = (List.take l n) @ (List.drop l (n + 1)) in
               let f = match front with
               | Some f' -> f'
               | None -> assert false (* n should never go outside the list *)
               and permuted_backs = list_permutations back in
               let permuted_lists = List.map ~f:(fun b -> f :: b) permuted_backs in
               inner_loop (n + 1) (List.append acc permuted_lists)
  in
  match list_length with
  | 0 | 1 -> [l]
  | _ -> inner_loop 0 []
;;

let intersperse_strings strings ~sep =
  strings
  |> List.intersperse ~sep
  |> List.map ~f:String.to_list
  |> List.join
  |> String.of_char_list
;;

let () =
  let args = Array.to_list (Array.slice Sys.argv 1 0) in
  let permutations = list_permutations args in
  List.iter ~f:(fun p -> print_endline (intersperse_strings p ~sep:" ")) permutations
;;
