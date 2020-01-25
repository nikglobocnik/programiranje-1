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



