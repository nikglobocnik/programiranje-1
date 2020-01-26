type 'a mm_drevo = 
  | Empty
  | Node of 'a mm_drevo * 'a * int * 'a mm_drevo

let rec vstavi x = function
  | Empty -> Node(Empty, x, 1, Empty)
  | Node(lt, y, num, rt) when y = x -> Node(lt, y, num + 1, rt)
  | Node(lt, y, num, rt) when y > x -> Node(vstavi x lt, y, num, rt)
  | Node(lt, y, num, rt) -> Node(lt, y, num, vstavi x rt)

let rec multimnozica_iz_seznama sez = 
  List.fold_left (fun mmnozica x -> vstavi x mmnozica) Empty sez

let rec velikost_multimnozice = function
  | Empty -> 0
  | Node(lt, y, num, rt) -> (velikost_multimnozice lt) + num + (velikost_multimnozice rt)

let seznam_iz_multimnozice mmtree = 
  let rec ponovi x n = if n <= 0 then [] else x :: ponovi x (n-1) in
  let rec v_seznam = function
    | Empty -> []
    | Node(lt, y, num, rt) -> (v_seznam lt) @ ponovi y num @ (v_seznam rt)
  in
  v_seznam mmtree

let a = multimnozica_iz_seznama [2; 5; 1; 4; 1; 1; 2; 8; 8]

 