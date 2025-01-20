module FsLatte.App.Program
open System
open System.Diagnostics
open System.IO
open FsLatte.Model.Abs
open FsLatte.Front
open FsLatte.Back
open FsLatte.Semantics
open FsLatte.Optimalization
open FSharpx
open Chessie.ErrorHandling

let private isLinux = 
    let p = (int) Environment.OSVersion.Platform;
    p = 4 || p = 6 || p = 128

let private getOutputFilePath (filePath:string) : string =
    Path.ChangeExtension (filePath, ".ll")

let runLlvmAs  outputFilePath =
    if isLinux then
        use p = new Process()
        p.StartInfo.FileName  <- "bash"
        p.StartInfo.Arguments <- " -c 'llvm-as " + outputFilePath + "'"
        p.StartInfo.WindowStyle <- System.Diagnostics.ProcessWindowStyle.Hidden
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardOutput <- true
        p.StartInfo.RedirectStandardError <- true
        p.Start () |> ignore
        p.StandardOutput.ReadToEnd() |> printfn "%s"
        p.StandardError.ReadToEnd()  |> printfn "%s"
        use so = Console.OpenStandardOutput()
        use se = Console.OpenStandardError ()
        p.StandardOutput.BaseStream.CopyToAsync so |> ignore
        p.StandardError.BaseStream.CopyToAsync se |> ignore
        so.Flush()
        se.Flush()
        p.WaitForExit () |> ignore
        so.Flush()
        se.Flush()
    else
        let logFilePath = Path.ChangeExtension (outputFilePath, "cmpout")
        //HACKs
        let basePath = AppDomain.CurrentDomain.BaseDirectory
        let compileScriptPath =  basePath + "compile.sh"
        let runtimeFilePath =  basePath + "runtime.bc"
        let runtimeFilePath2 =  basePath + "runtime_extension.bc"

        let outputFilePath = outputFilePath.Replace('\\', '/').Replace("C:", "/mnt/c")
        let compileScriptPath = compileScriptPath.Replace('\\', '/').Replace("C:", "/mnt/c")
        let runtimeFilePath = runtimeFilePath.Replace('\\', '/').Replace("C:", "/mnt/c")
        let runtimeFilePath2 = runtimeFilePath2.Replace('\\', '/').Replace("C:", "/mnt/c")

        let p = new Process()
        p.StartInfo.FileName  <- "bash"
        p.StartInfo.Arguments <- compileScriptPath + " " + outputFilePath + " " + runtimeFilePath + " " + runtimeFilePath2
        p.StartInfo.WindowStyle <- System.Diagnostics.ProcessWindowStyle.Hidden
        p.StartInfo.CreateNoWindow <- true
        p.Start () |> ignore
        p.WaitForExit() |> ignore
        p.Dispose()
        File.ReadAllText logFilePath |> printfn "%s"
        File.Delete logFilePath

let exnLift f x = Catch f x |> mapFailure (List.map (fun x -> x.Message))

let getFileContent (path:string) = path |> exnLift File.ReadAllText 
let parse (code:string)   = code |> LatteParser.parseFromString
let verify (prog:Program) = prog |> SemanticChecker.check
let latCompile (prog:Program) =
    prog
    |> IntermediateFormGenerator.generate
    |> lift (uncurry LlvmILCodeGenerator.generate)

let saveLatCode (path:string) (code:string) = code |> exnLift (curry File.WriteAllText path)
let llvmCompile (path:string) () = runLlvmAs path
let printLatCode (code:string) =
    printfn "%s" code
    code

let processFile (inputfilePath:string) : int =
    let outputFilePath = inputfilePath |> getOutputFilePath

    inputfilePath
    |> getFileContent 
    |> bind parse
    //|> successTee print (FsLatte.Front.App.Program.showTree prog >> printfn "%s")
    |> lift Optimalize.computeConsts
    //|> successTee (FsLatte.Front.App.Program.showTree prog >> printfn "%s")
    |> bind verify
    |> successTee (fun _ -> eprintfn "OK")
    |> failureTee (fun errors -> eprintfn "ERROR"; printfn "%A" errors;)
    |> bind latCompile
    //|> lift printLatCode
    |> bind (saveLatCode outputFilePath)
    |> lift (llvmCompile outputFilePath)
    |> either (fun _ -> 0) (fun errors -> 2)

[<EntryPoint>]
let main args =
    if Array.length args < 1 then
        eprintfn "No file path provided"
        1
    else
        processFile args.[0]
