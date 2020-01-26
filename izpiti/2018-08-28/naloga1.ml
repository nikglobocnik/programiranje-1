let razlika_kvadratov a b = (a + b)*(a + b) - a * a - b * b

let uporabi_na_paru f (x, y) = (f(x), f(y))

let rec ponovi_seznam n lst = 
  if n <= 0 then [] else lst @ ponovi_seznam (n-1) lst

let rec razdeli sez =
  let rec aux acc1 acc2 = function
    | [] -> (List.rev acc1, List.rev acc2)
    | x :: xs when x < 0 -> aux (x :: acc1) acc2 xs
    | x :: xs -> aux acc1 (x :: acc2) xs
  in 
  aux [] [] sez
