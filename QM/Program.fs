// For more information see https://aka.ms/fsharp-console-apps
open QM.Implementation

let g = QMTerm.FromInt 4
let f = [0; 1; 2; 4; 5; 6; 9; 10]

simplify (f |> List.map g) |> ignore
