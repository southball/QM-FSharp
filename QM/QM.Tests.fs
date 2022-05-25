namespace QM.Tests

open System
open QM.Implementation
open NUnit.Framework

[<TestFixture>]
type QMTests () =
    [<Test>]
    member this.QMTermMergeTest() =
        let term1 = QMTerm.Term [| Zero; One; Zero; Dash |]
        let term2 = QMTerm.Term [| Zero; One; One; Dash |]
        let term3 = QMTerm.Term [| One; One; Zero; Dash |]
        Assert.True <| QMTerm.CanMerge term1 term2
        Assert.True <| QMTerm.CanMerge term1 term3
        Assert.False <| QMTerm.CanMerge term2 term3