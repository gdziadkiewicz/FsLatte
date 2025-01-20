namespace FsLatte.Semantics
open Chessie.ErrorHandling
open FsLatte.Model.Abs
open FSharpx

module public SemanticChecker =
    let public check (program:Program) : CheckResult =
        let runAndConcat (checkers: (Program->string list) list) (program: Program) : string list =
            checkers |> List.collect (fun checker -> checker program)

        let checkers = [
            TypeChecker.checkTypes
            EntryPointChecker.checkMain
            FunctionChecker.checkFunctionsRedefinition
            FunctionChecker.checkFunctionsArgs
            ReturnChecker.checkIfFunctionsHaveReturns
            FunctionChecker.checkVariablesRedefinitions
            ]
        let errors = program |>  runAndConcat checkers
        if List.isEmpty errors then
            ok program
        else
            Bad errors
