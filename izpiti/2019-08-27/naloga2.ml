type 'a gnezdenje = 
  | Element of 'a
  | Podseznam of 'a gnezdenje list

let gnezdenje_primer = [Element 1; Element 2; 
                    Podseznam [Element 3; Podseznam [Element 4]; Podseznam[]]; Podseznam [Element 5]]

let rec najvecja_globina = function
  | [] -> 1
  | Element _ :: xs -> najvecja_globina xs
  | Podseznam podsez :: xs ->
    max (najvecja_globina podsez + 1) (najvecja_globina xs)

let rec preslikaj f = function
  | [] -> []
  | Element x :: xs -> Element (f x) :: preslikaj f xs
  | Podseznam podsez :: xs -> Podseznam (preslikaj f podsez) :: preslikaj f xs

let rec splosci = function
  | [] -> []
  | Element x :: xs -> x :: splosci xs
  | Podseznam podsez :: xs -> splosci podsez @ splosci xs

let rec alternirajoci_konstruktorji = function
  | [] | [_] -> true
  | Element _ :: Podseznam _ :: xs -> alternirajoci_konstruktorji xs
  | Podseznam _ :: Element _ :: xs -> alternirajoci_konstruktorji xs
  | Element _ :: Element _ :: _ -> false
  | Podseznam _ :: Podseznam _ :: _ -> false