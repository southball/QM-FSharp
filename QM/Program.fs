// For more information see https://aka.ms/fsharp-console-apps
open QM.Implementation
open QM.Utilities

printf "Please enter number of bits: "
let numberOfBits = System.Console.ReadLine() |> int
let toQMTerm = QMTerm.FromInt numberOfBits

printf "Please enter minterms, separated by space or comma: "

let minterms =
    System.Console.ReadLine().Split([| ' '; ',' |])
    |> Array.filter (String.length >> (<>) 0)
    |> Array.map (int >> toQMTerm)
    |> Array.toList

printf "Please enter don't-care terms, separated by space or comma: "

let dcterms =
    System.Console.ReadLine().Split([| ' '; ',' |])
    |> Array.filter (String.length >> (<>) 0)
    |> Array.map (int >> toQMTerm)
    |> Array.toList

let solution = simplify minterms dcterms
printfn ""

printfn
    "%s"
    (solution
     |> List.map debugFormatPrimeImplicant
     |> String.concat " + ")
