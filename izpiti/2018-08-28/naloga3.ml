type 'a veriga = | Filter of ('a -> bool) * 'a list * 'a veriga
                 | Ostalo of 'a list

let test = Filter((>)0, [],
           Filter((>)10, [],
           Ostalo [] ))
           
let test1 = Filter((>)0, [-5;-7],
           Filter((>)10, [7;2],
           Ostalo [100] ))

let rec vstavi x = function
  | Ostalo sez -> Ostalo (x :: sez)
  | Filter(f, sez, veriga) when f x -> Filter(f, x :: sez, veriga)
  | Filter(f, sez, veriga) -> Filter(f, sez, vstavi x veriga)

let rec poisci x =  function
  | Ostalo sez -> List.mem x sez
  | Filter (f, sez, veriga) when f x -> List.mem x sez
  | Filter (f, sez, veriga) -> poisci x veriga

let rec izprazni_filtre = function
  | Ostalo sez -> (Ostalo [], sez)
  | Filter (f, sez, veriga) -> 
      let (prazni, vsebina) = izprazni_filtre veriga in
      (Filter (f, [], prazni), sez @ vsebina) 

let rec dodaj_filter f veriga = 
  let (prazni, vsebina) = izprazni_filtre veriga in
  let nova_veriga = Filter (f, [], prazni) in
  List.fold_right vstavi vsebina nova_veriga