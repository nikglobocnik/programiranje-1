type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  (* Tip linearnih preslikav *)
  type t
  (* Identiteta *)
  val id : t
  (* Dano preslikavo uporabi na vektorju *)
  val uporabi : t -> vektor -> vektor
  (* Vrne linearno preslikavo, določeno z matriko *)
  val iz_matrike : matrika -> t
  (* Vrne linearno preslikavo, določeno s funkcijo
     (predpostavite lahko, da je funkcija linearna) *)
  val iz_funkcije : (vektor -> vektor) -> t
  (* Vrne kompozitum danih preslikav. *)
  val kompozitum : t -> t -> t
end

module Matrika : Linearna = struct
  type t = int*int*int*int

  let id = (1, 0, 0, 1)
  let uporabi (a, b, c, d) (x, y)  = (x*a + y*b, x*c + y*d)
  let iz_matrike m = m
  let iz_funkcije f =
    let (a, c) = f (1, 0) in
    let (b, d) = f (0, 1) in
    (a, b, c, d)
  let kompozitum (a, b, c, d) (e, f, g, h) =
    (a*e + b*g, a*f + b*h, c*e + d*g, c*f + c*h)
end

module Funkcija : Linearna = struct
  type t = int*int -> int*int

  let id = (fun x -> x)
  let uporabi f v = f v
  let iz_matrike (a, b, c, d) = fun (x, y) -> (x*a + y*b, x*c + y*d)
  let iz_funkcije f = f
  let kompozitum  f g = fun x -> f(g(x))
end
