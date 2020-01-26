type 'a tree = Empty
             | Node of 'a tree * 'a * 'a tree

let daljsi lst1 lst2 =
  if List.length lst1 > List.length lst2 then lst1 else lst2

let rec narascajoca sp_meja = function
  | Empty -> []
  | Node (l, x, r) when x > sp_meja -> []
  | Node (l, x, r) -> x :: daljsi (narascajoca x l) (narascajoca x r)

let rec padajoca zg_meja = function
  | Empty -> []
  | Node (l, x, r) when x < zg_meja -> []
  | Node (l, x, r) -> x :: daljsi (padajoca x l) (padajoca x r)

let rec monotona_pot = function
  | Empty -> []
  | Node (l, x, r) ->
      let skupaj1 = (padajoca x l) @ [x] @ (narascajoca x r) in
      let skupaj2 = (narascajoca x l) @ [x] @ (padajoca x r) in
      let najdaljsi_skupaj = daljsi skupaj1 skupaj2 in
      let najdaljsi_poddrevesa = daljsi (monotona_pot l) (monotona_pot r) in
      daljsi najdaljsi_skupaj najdaljsi_poddrevesa