module QM.Implementation

open QM.Utilities

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

    static member IsSubsetOf (bool1: QMBool) (bool2: QMBool) =
        match bool1, bool2 with
        | b1, b2 when b1 = b2 -> true
        | _, Dash -> true
        | _ -> false

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

    static member Length(term: QMTerm) : int = term |> QMTerm.Unwrap |> Array.length

    static member CanMerge (term1: QMTerm) (term2: QMTerm) : bool =
        let Term term1, Term term2 = term1, term2

        assert (Array.length term1 = Array.length term2)

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

        assert (Array.length term1 = Array.length term2)

        Array.init (Array.length term1) (fun i -> QMBool.Merge term1.[i] term2.[i])
        |> QMTerm.Term

    static member CountWhere (predicate: QMBool -> bool) (term: QMTerm) : int =
        term
        |> QMTerm.Unwrap
        |> Array.filter predicate
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

    static member ToInt(term: QMTerm) : int option =
        Array.foldBack
            (fun t s ->
                if t = Dash then
                    None
                else
                    Option.map (fun s -> (if t = One then 1 else 0) + s * 2) s)
            (QMTerm.Unwrap term)
            (Some 0)

    static member IsSubsetOf (term1: QMTerm) (term2: QMTerm) : bool =
        assert (QMTerm.Length term1 = QMTerm.Length term2)
        Array.forall2 (fun e1 e2 -> QMBool.IsSubsetOf e1 e2) (QMTerm.Unwrap term1) (QMTerm.Unwrap term2)

let debugFormatQMTerms terms =
    terms
    |> List.groupBy (QMTerm.CountWhere((=) One))
    |> List.sort
    |> List.map (
        snd
        >> List.map (fun x ->
            match QMTerm.ToInt x with
            | Some i -> sprintf "%s (%d)" (string x) (i)
            | None -> string x)
        >> String.concat ", "
    )
    |> String.concat "; "

let debugFormatPrimeImplicant term =
    let (Term term) = term

    term
    |> Array.rev
    |> Array.indexed
    |> Array.map (mapFst ((+) 1))
    |> Array.filter (snd >> (<>) Dash)
    |> Array.map (fun (index, qmBool) -> sprintf "x%d%s" index (if qmBool = Zero then "'" else ""))
    |> String.concat " "

let debugFormatPrimeImplicants terms =
    terms
    |> List.map debugFormatPrimeImplicant
    |> String.concat ", "

let rec findPrimeImplicants (terms: QMTerm list) : QMTerm list =
    let iterate (terms: QMTerm list) : (QMTerm list * QMTerm list) =
        let termsByCount1 =
            terms
            |> List.groupBy (QMTerm.CountWhere((=) One))
            |> List.sortBy fst
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

        printfn "Terms: %s" (debugFormatQMTerms terms)
        printfn "Unused: %s" (debugFormatQMTerms unusedTerms)
        printfn "New: %s" (debugFormatQMTerms newTerms)
        printfn ""

        if newTerms.IsEmpty then
            unusedTerms
        else
            unusedTerms @ getAll newTerms

    getAll terms |> List.distinct

let simplify (minterms: QMTerm list) (dcterms: QMTerm list) : QMTerm list =
    if minterms.Length = 0 then
        failwith "minterms must not be empty!"

    let bitCount = minterms |> List.head |> QMTerm.Length

    assert
        (minterms @ dcterms
         |> List.forall (QMTerm.Length >> (=) bitCount))

    let primeImplicants = findPrimeImplicants (minterms @ dcterms)

    let isBetterSolution solution1 solution2 : int =
        let termLengthList solution =
            solution
            |> List.map (QMTerm.CountWhere((<>) Dash))
            |> List.sort

        let c = compare (List.length solution1) (List.length solution2)

        if c <> 0 then
            c
        else
            compare (termLengthList solution1) (termLengthList solution2)

    let solveByBruteForce minterms primeImplicants : QMTerm list =
        primeImplicants
        |> Set
        |> allSubsets
        |> Seq.map Set.toList
        |> Seq.filter (fun implicants ->
            minterms
            |> List.forall (fun minterm ->
                implicants
                |> Seq.exists (QMTerm.IsSubsetOf minterm)))
        |> Seq.toList
        |> List.sortWith isBetterSolution
        |> List.head


    let rec iterate minterms primeImplicants : QMTerm list =
        if List.isEmpty minterms then
            []
        else
            let primeImplicantChart =
                primeImplicants
                |> List.map (fun implicant -> implicant, List.filter (flip QMTerm.IsSubsetOf implicant) minterms)
                |> List.filter (snd >> List.length >> (<>) 0) // Remove completely empty rows

            let primeImplicantChartTransposed =
                minterms
                |> List.map (fun minterm -> minterm, List.filter (QMTerm.IsSubsetOf minterm) primeImplicants)

            printfn "Prime implicant chart:"

            for i in primeImplicantChart do
                printfn "  %O" i

            let essentialPrimeImplicants =
                primeImplicantChartTransposed
                |> List.filter (snd >> List.length >> (=) 1)
                |> List.map (snd >> List.head)
                |> List.distinct

            printfn "Essential prime implicants: %O" (debugFormatPrimeImplicants essentialPrimeImplicants)

            let remainingMinterms =
                minterms
                |> List.filter (
                    not
                    << flip List.exists essentialPrimeImplicants
                    << QMTerm.IsSubsetOf
                )

            printfn "Remaining minterms: %O" (debugFormatQMTerms remainingMinterms)

            let essentialPrimeImplicantsSet = Set essentialPrimeImplicants

            let remainingPrimeImplicants =
                primeImplicantChart
                |> List.filter (snd >> List.length >> flip (>=) 1)
                |> List.filter (
                    not
                    << flip Set.contains essentialPrimeImplicantsSet
                    << fst
                )
                |> List.map fst

            printfn "Remaining prime implicants: %O" (debugFormatPrimeImplicants remainingPrimeImplicants)

            let newPrimeImplicantChart =
                remainingPrimeImplicants
                |> List.map (fun implicant ->
                    implicant,
                    List.filter (flip QMTerm.IsSubsetOf implicant) remainingMinterms
                    |> Set)
                |> Map

            let nonDominatedPrimeImplicant =
                remainingPrimeImplicants
                |> List.indexed
                |> List.filter (fun (index1, implicant1) ->
                    remainingPrimeImplicants
                    |> List.indexed
                    |> List.exists (fun (index2, implicant2) ->
                        index1 < index2
                        && Set.isSubset
                            (Map.find implicant1 newPrimeImplicantChart)
                            (Map.find implicant2 newPrimeImplicantChart))
                    |> not)
                |> List.map snd

            printfn "Non-dominated prime implicants: %O" (debugFormatPrimeImplicants nonDominatedPrimeImplicant)
            printfn ""

            if Set nonDominatedPrimeImplicant
               <> Set primeImplicants then
                essentialPrimeImplicants
                @ iterate remainingMinterms nonDominatedPrimeImplicant
            else
                solveByBruteForce minterms primeImplicants

    iterate minterms primeImplicants
