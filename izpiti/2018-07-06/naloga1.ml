let uporabi f x = f x

let ibaropu x f = f x

let rec zacetnih n xs =
  let rec zacetnih' n xs acc =
    if n <= 0 then
      Some (List.rev acc)
    else
    match xs with
    | [] -> None
    | x :: xs -> zacetnih' (n-1) xs (x :: acc)
  in
  zacetnih' n xs []