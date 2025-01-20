module FsLatte.Tests.SemanticsTests
open FsLatte.Semantics
open NUnit.Framework
open FsLatte.Tests.Data
open FsLatte.Front
open Chessie.ErrorHandling

[<Test>]
[<Category("Pesimistic")>]
[<TestCaseSource( typeof<TestDataSets> ,"ErrorsFiles")>]
let ``Error files should return failure`` (td:TestData) : unit =
    td.Content
    |> LatteParser.parseFromString
    |> failureTee (fun errors -> Assert.Fail ("Parsing failed with:\n" + String.concat "\n" errors ))
    |> bind SemanticChecker.check
    |> lift (fun _ -> Assert.Fail())
    |> failureTee (fun errors -> errors |> printfn "%A")
    |> ignore