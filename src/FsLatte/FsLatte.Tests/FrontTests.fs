module FsLatte.Tests.FrontTests
open NUnit.Framework
open FsUnit
open Swensen.Unquote
open System.Collections
open System.IO
open FsLatte.Tests.Data
open System
open FsLatte.Front
open FsLatte.Front.App.Program
open Chessie.ErrorHandling

[<Test>]
[<Category("Optimistic")>]
[<TestCaseSource( typeof<TestDataSets> ,"GoodFiles")>]
let ``Good files should pass parsing`` (td:TestData) =
    td.Content
    |> LatteParser.parseFromString
    |> eitherTee
        (fun x ->
            x
            |>fst
            |> showTree
            |> printfn "%s")
        (fun errors -> Assert.Fail ("Parsing failed with:\n" + String.concat "\n" errors ))
    |> ignore

[<Test>]
[<Category("Pesimistic")>]
[<TestCaseSource( typeof<TestDataSets> ,"SyntaxErrorsFiles")>]
let ``Syntax error files should return failure`` (td:TestData) : unit =
    td.Content
    |> LatteParser.parseFromString
    |> eitherTee
        (fun (prog,_) ->
            prog
            |> showTree
            |> sprintf "Parsing wrongly succeded with:\n%s"
            |> Assert.Fail)
        (fun errors -> errors |> printfn "%A")
    |> ignore