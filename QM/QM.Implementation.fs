module QM.Implementation

type QMBool =
    | Zero
    | One
    | Dash
    override this.ToString() =
        match this with
        | Zero -> "0"
        | One -> "1"
        | Dash -> "-"

    static member Merge (bool1: QMBool) (bool2: QMBool) =
        match bool1, bool2 with
        | (b1, b2) when b1 = b2 -> b1
        | (Zero, One) -> Dash
        | (One, Zero) -> Dash
        | _ -> failwith "Cannot merge!"

type QMTerm =
    | Term of QMBool array

    static member private Unwrap(term: QMTerm) : QMBool array =
        let (Term unwrapped) = term
        unwrapped

    override this.ToString() =
        this
        |> QMTerm.Unwrap
        |> Array.map (fun x -> x.ToString())
        |> Array.rev
        |> String.concat ""

    static member CanMerge (term1: QMTerm) (term2: QMTerm) : bool =
        let Term term1, Term term2 = term1, term2

        if Array.length term1 <> Array.length term2 then
            failwith "Incompatible length"

        let countZeroOnes =
            Array.map2 (fun e1 e2 -> (e1, e2) = (Zero, One) || (e1, e2) = (One, Zero)) term1 term2
            |> Array.sumBy System.Convert.ToInt32

        let countEquals =
            Array.map2 (=) term1 term2
            |> Array.sumBy System.Convert.ToInt32

        countZeroOnes = 1
        && countEquals = (Array.length term1) - 1

    static member Merge (term1: QMTerm) (term2: QMTerm) : QMTerm =
        let term1, term2 = term1 |> QMTerm.Unwrap, term2 |> QMTerm.Unwrap

        if Array.length term1 <> Array.length term2 then
            failwith "Incompatible length"

        Array.init (Array.length term1) (fun i -> QMBool.Merge term1.[i] term2.[i])
        |> QMTerm.Term

    static member Count1(term: QMTerm) : int =
        term
        |> QMTerm.Unwrap
        |> Array.filter ((=) One)
        |> Array.length

    static member FromInt (length: int) (value: int) : QMTerm =
        let rec f l v =
            if l = 0 then
                []
            else
                (if v % 2 = 0 then
                     QMBool.Zero
                 else
                     QMBool.One)
                :: f (l - 1) (v / 2)

        f length value |> List.toArray |> QMTerm.Term

let rec simplify (terms: QMTerm list) : QMTerm list =
    let iterate (terms: QMTerm list) : (QMTerm list * QMTerm list) =
        let termsByCount1 =
            List.groupBy QMTerm.Count1 terms
            |> List.sortBy (fun (count1, _) -> count1)
            |> List.toSeq

        let (usedTerms, newTerms) =
            termsByCount1
            |> Seq.windowed 2
            |> Seq.fold
                (fun (usedTerms, newTerms) window ->
                    match window with
                    | [| (count1, terms1); (count2, terms2) |] ->
                        if count1 + 1 <> count2 then
                            usedTerms, newTerms
                        else
                            List.allPairs terms1 terms2
                            |> List.fold
                                (fun (usedTerms, newTerms) (term1, term2) ->
                                    if not (QMTerm.CanMerge term1 term2) then
                                        usedTerms, newTerms
                                    else
                                        (usedTerms |> Set.add term1 |> Set.add term2,
                                         QMTerm.Merge term1 term2 :: newTerms))
                                (usedTerms, newTerms)
                    | _ -> failwith "Unexpected")
                (Set [], [])

        Set.difference (Set terms) usedTerms |> Set.toList, List.distinct newTerms

    let rec getAll terms =
        let unusedTerms, newTerms = iterate terms

        let format terms =
            terms
            |> List.groupBy QMTerm.Count1
            |> List.sort
            |> List.map (fun (_, terms) -> terms |> List.map string |> String.concat ", ")
            |> String.concat "; "

        printfn "Terms: %s" (format terms)
        printfn "Unused: %s" (format unusedTerms)
        printfn "New: %s" (format newTerms)
        printfn ""

        if newTerms.IsEmpty then
            unusedTerms
        else
            unusedTerms @ getAll newTerms

    getAll terms |> List.distinct
