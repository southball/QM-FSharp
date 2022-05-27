module QM.Utilities

let flip<'A, 'B, 'C> (f: 'A -> 'B -> 'C) (x: 'B) (y: 'A) = f y x

let mapFst<'A, 'B, 'C> (f: 'A -> 'C) ((p, q): 'A * 'B) = f p, q
let mapSnd<'A, 'B, 'C> (f: 'B -> 'C) ((p, q): 'A * 'B) = p, f q

let rec allSubsets<'T when 'T: comparison> (s: Set<'T>) : Set<'T> seq =
    set [ yield s
          for e in s do
              yield! allSubsets<'T> (Set.remove e s) ]
