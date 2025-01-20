module FsLatte.Front.App.Program
open System
open System.IO
open FsLatte.Model.Abs
open FsLatte.Front
open Print
open Show
open Chessie.ErrorHandling

let showTree (t:Program) : string =
    "[Abstract syntax]\n\n" +
    (t |> showProgram |> show) +
    "\n\n" +
    "[Linearized tree]\n\n" +
    printTree prtProgram t +
    "\n"

[<EntryPoint>]
let main args =
    use channel =
        if Array.length args > 0 then File.OpenText args.[0] :> TextReader
        else stdin
    channel
    |> LatteParser.parseFromTextReader
    |> Trial.either
       (fun (prog,_) ->
            prog
            |> showTree
            |> printfn "%s"
            0)
       (fun errors ->
            errors |> List.iter (printfn "%s")
            1)
