namespace QM.Tests

open System
open QM.Implementation
open NUnit.Framework

[<TestFixture>]
type QMTests() =
    [<Test>]
    member this.QMTermMergeTest() =
        let term1 = QMTerm.Term [| Zero; One; Zero; Dash |]
        let term2 = QMTerm.Term [| Zero; One; One; Dash |]
        let term3 = QMTerm.Term [| One; One; Zero; Dash |]
        Assert.True <| QMTerm.CanMerge term1 term2
        Assert.True <| QMTerm.CanMerge term1 term3
        Assert.False <| QMTerm.CanMerge term2 term3

    [<Test>]
    member this.QMTermIntTest() =
        let term1 = QMTerm.FromInt 4 11
        let term2 = QMTerm.FromInt 6 1
        let term3 = QMTerm.FromInt 8 124
        Assert.AreEqual(11, QMTerm.ToInt term1)
        Assert.AreEqual(1, QMTerm.ToInt term2)
        Assert.AreEqual(124, QMTerm.ToInt term3)
