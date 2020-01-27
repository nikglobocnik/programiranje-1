type complex = { re : float ; im : float}

let complex_add z w = { re = z.re +. w.re ; im = z.im +. w.im }

let complex_conjugate {re ; im} = {re ; im = -. im}

let list_apply_either pred f g xs = 
  List.map (fun x -> if pred x then f x else g x) xs

let eval_poly koef x = 
  let rec aux acc koef x_n = 
    match koef with
    | [] -> acc
    | a :: koef -> aux (acc + a*x_n) koef (x*x_n)
  in
  aux 0 koef 1

