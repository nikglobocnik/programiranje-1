type filter_tree = Node of int * filter_tree * filter_tree
                 | Box of int list

let primer = Node (10, Node(5, (Box [1]), (Box [])), Node(15, (Box []), (Box [19; 20])))

let rec vstavi x fdrevo =
  match fdrevo with
  | Node (v, lt, rt) ->
      if v > x
      then Node (v, vstavi x lt, rt)
      else Node (v, lt, vstavi x rt)
  | Box (bs) -> Box (x::bs)


let vstavi_seznam lst fdrevo =
  List.fold_right vstavi lst fdrevo
  
let rec pravilno_vstavljen fdrevo = 
  let preveri sp zg x =
    match (sp, zg) with
    | (None, None) -> true
    | (Some l, None) -> l <= x
    | (None, Some r) -> x < r
    | (Some l, Some r) -> l <= x && x < r
  in 
  let rec vrednosti_med  sp zg fdrevo =
    match fdrevo with 
    | Box (bs) -> List.for_all (preveri sp zg) bs
    | Node(v, lt, rt) -> (vrednosti_med sp (Some v) lt) && (vrednosti_med (Some v) zg rt)
  in
  vrednosti_med None None fdrevo

  let primer1 = Node (10, Node(5, (Box [1]), (Box [])), Node(15, (Box [19]), (Box [19; 20])))

