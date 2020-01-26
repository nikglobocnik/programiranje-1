(* -------- 1 -------- *)

let rec vsota sez =
  let rec vsota_aux acc = function
    | [] -> acc
    | x :: xs -> vsota_aux (x + acc) xs
  in
  vsota_aux 0 sez

(* -------- 2 -------- *)

let rec je_urejen = function
  | [] | [_] -> true
  | x :: y :: ys -> x < y && je_urejen (y :: ys)

(* -------- 3 -------- *)

let rec vstavi x lst = 
  match lst with
  | [] -> [x]
  | y :: ys -> if x <= y
               then 
               x :: lst 
               else 
               y :: (vstavi x ys)

let rec uredi = function
  | [] -> []
  | x :: xs -> vstavi x (uredi xs)

(* -------- 4 -------- *)

let rec vstavi2 cmp x lst =
  match lst with
  | [] -> [x]
  | y :: ys -> if cmp x y
               then
               x :: lst
               else
               y :: (vstavi2 cmp x ys)

let rec uredi_z cmp = function
  | [] -> []
  | x :: xs -> vstavi2 cmp x (uredi_z cmp xs)

(* -------- 5 -------- *)
type priority = Top | Group of int
type status = Staff | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)

let primerjaj_potnika x y =
  match (x.status, y.status) with
  | (Staff, _) -> true
  | (_, Staff) -> false
  | (Passenger Top, _) -> true
  | (Passenger _, Passenger Top) -> false
  | (Passenger Group j, Passenger Group k) -> j > k

let uredi_potnike seznam = uredi_z primerjaj_potnika seznam


(* -------- 7 -------- *)

let razdeli_v_bloke sez = 
  let zaporedje = uredi_potnike sez in
  let rec aux bloki blok1 stat = function
    | [] -> blok1 :: bloki
    | p :: ps ->
      if stat = p.status then
        aux bloki (p :: blok1) stat ps
      else
        aux (blok1 :: bloki) [p] p.status ps
  in
  match zaporedje with
  | [] -> [[]]
  | f :: fs -> 
    let bloki = aux [] [f] f.status fs in
    List.rev bloki