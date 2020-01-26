let podvoji_vsoto a b = 2 * (a + b)

let povsod_vecji (a, b, c) (d, e, f) = a > d && b > e && c > f

let uporabi_ce_lahko f = function
  | None -> None
  | Some x -> Some (f x)

let rec pojavi_dvakrat x lst = 
  let rec prestej = function
    | [] -> 0
    | y :: ys -> if x = y then 1 + (prestej ys) else prestej ys
  in
  prestej lst = 2

let izracunaj_v_tocki x sez_fun =
  let rec aux acc = function
    | [] -> List.rev []
    | f :: fs -> aux ((f x) :: acc) fs
  in
  aux [] sez_fun

let rec eksponent x p =
  let rec aux x p acc = 
    if p <= 0 then acc else aux x (p-1) (x * acc)
  in
  aux x p 1
