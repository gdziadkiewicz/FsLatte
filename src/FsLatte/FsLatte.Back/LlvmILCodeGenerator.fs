module FsLatte.Back.LlvmILCodeGenerator
open System
open FsLatte.Model.IntermediateForm
open FSharpx.Strings
open FSharpx.Prelude

let private newline = Environment.NewLine
let private unlines = separatedBy newline

let private makeLine (line:LlvmLine) : string = line |> toCode

let private makeFunction (function':Function) : string =
    let (functionName, (returnType, arguments), bodyLines) = function'
    let functionName = stringifyValue functionName
    let returnType' =  returnType |> stringifyType
    let arguments' =
        arguments
        |> List.map (fun (argumentType, argumentName) -> stringifyType argumentType + " " + stringifyValue argumentName )
        |> separatedBy ", "
    let body = bodyLines |> List.map makeLine |> unlines 
    sprintf """define %s %s (%s) {
%s
}""" returnType' functionName arguments' body

let private makeStringLiteral (literal : string) id =
    let id' = stringifyValue id
    let literalLength = String.length literal + 1
    sprintf "%s = internal constant [%i x i8] c\"%s\\00\"" id' literalLength literal

let private makeFunctions (functions : Function list) : string =
    functions |> List.map makeFunction |> unlines 
let private makeStringLiterals (literals : (string*Value) list) : string =
    literals |> List.map (uncurry makeStringLiteral) |> unlines

let private prologDeclarations : string =
    """
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @concat(i8*, i8*)
"""

let generate (funcs : Function list) (stringLiterals : (string*Value) list) : string =
    let funcsCode = makeFunctions funcs
    let stringsCode = makeStringLiterals stringLiterals
    unlines
        [
        prologDeclarations
        stringsCode
        funcsCode
        ]
