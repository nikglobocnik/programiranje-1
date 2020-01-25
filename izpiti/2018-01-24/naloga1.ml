let rec izpisi_vsa_stevila l =
  match l with
  | [] -> ()
  | x :: xs -> print_int x; izpisi_vsa_stevila xs

let rec mat2_opt f lst1 lst2 =
  let rec aux lst1 lst2 f acc =
    match (lst1, lst2) with
    | ([], []) -> Some (List.rev acc)
    | ([], _) -> None
    | (_, []) -> None
    | (x :: xs, y :: ys) -> aux xs ys f ((f x y) :: acc)
  in
  aux lst1 lst2 f []
