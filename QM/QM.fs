module QM

type QMBool =
    | Zero
    | One
    | Dash
    static member Merge (bool1: QMBool) (bool2: QMBool) =
        match bool1, bool2 with
        | (b1, b2) when b1 = b2 -> b1
        | (Zero, One) -> Dash
        | (One, Zero) -> Dash
        | _ -> failwith "Cannot merge!"

type QMTerm =
    | Term of QMBool array
    static member Unwrap(term: QMTerm) : QMBool array =
        let (Term self) = term
        self

    static member CanMerge (term1: QMTerm) (term2: QMTerm) : bool =
        let term1, term2 = term1 |> QMTerm.Unwrap, term2 |> QMTerm.Unwrap

        if Array.length term1 <> Array.length term2 then
            failwith "Incompatible length"

        0 = Array.fold2
                (fun count e1 e2 ->
                    count
                    + if (e1, e2) = (Zero, One) || (e1, e2) = (One, Zero) then
                          1
                      else
                          0)
                0
                term1
                term2

    static member Merge (term1: QMTerm) (term2: QMTerm) : QMTerm =
        let term1, term2 = term1 |> QMTerm.Unwrap, term2 |> QMTerm.Unwrap

        if Array.length term1 <> Array.length term2 then
            failwith "Imcompatible length"
        
        Array.init (Array.length term1) (fun i -> QMBool.Merge term1.[i] term2.[i]) |> QMTerm.Term

    static member Count1(term: QMTerm) : int =
        term
        |> QMTerm.Unwrap
        |> Array.filter ((=) One)
        |> Array.length

let rec simplify (terms: QMTerm list): QMTerm list =
    let termsByCount1 = List.groupBy QMTerm.Count1 terms |> List.sortBy (fun (count1, _) -> count1) |> List.toSeq
    let (usedSet, allSet) = 
        termsByCount1 |> Seq.windowed 2
        |> Seq.fold (fun (usedSet, allSet) window ->
            match window with
            | [| (count1, terms1); (count2, terms2) |] ->
                if count1 + 1 <> count2
                then usedSet, allSet
                else 
            | _ -> failwith "Unexpected"
        ) (Set [], Set [])
    allSet
