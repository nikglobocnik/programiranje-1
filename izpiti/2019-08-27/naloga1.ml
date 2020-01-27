let odstej_trojici (a, b, c) (d, e, f) = (a - d, b - e, c - f)

let rec max_rezultat_do_n f n = 
  if n <= 0 then
    f 0
  else
    max (f n) (max_rezultat_do_n f (n-1))

let rec pocisti_seznam list = 
  let rec aux acc = function
    | [] -> List.rev acc
    | None :: xs -> aux acc xs
    | Some x :: xs -> aux (x :: acc) xs
  in
  aux [] list

let rec preveri_urejenost list = 
  let rec narasca = function
    | [] | [_] -> true
    | x1 :: x2 :: xs -> if x1 < x2  then narasca (x2 :: xs) else false
  in
  (list |> List.filter (fun x -> x mod 2 == 0) |> narasca)
  &&
  (list |> List.filter (fun x -> x mod 2 == 1) |> List.rev |> narasca)

