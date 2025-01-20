module FsLatte.Tests.End2EndTests
open System.Diagnostics
open NUnit.Framework
open Swensen.Unquote
open FsUnit
open System.Collections
open System.IO
open FsLatte.Tests.Data
open System.IO
open FsLatte.App
open System

[<Test>]
[<Category("Optimistic")>]
[<Category("E2E")>]
[<TestCaseSource( typeof<TestDataSets> ,"GoodFiles")>]
let ``Good files should compile and have expected output`` (code:TestData) =
    let expectedOutput = code.ExpectedOutput
    let tempFile = Path.GetTempFileName()
    let outputLlvmFile = Path.ChangeExtension(tempFile, "ll")
    let outputBcFile = Path.ChangeExtension(tempFile, "bc")
    let outputFile= Path.ChangeExtension(tempFile, "out")
    File.WriteAllText(tempFile, code.Content)
    
    let exitCode = Program.main [|tempFile|]
    
    Assert.AreEqual(0, exitCode,"Exit code should be equal to 0")
    test <@ File.Exists outputLlvmFile @>
    test <@ File.Exists outputBcFile @>

    let x = new Process() 
    x.StartInfo.FileName <- "bash"

    let scriptPath = "run.sh"
    let basePath = AppDomain.CurrentDomain.BaseDirectory
    let scriptPath =  basePath + scriptPath
    let scriptPath = scriptPath.Replace('\\', '/').Replace("C:", "/mnt/c")
    let outputBcFileLinux = outputBcFile.Replace('\\', '/').Replace("C:", "/mnt/c")
    x.StartInfo.Arguments <- scriptPath + " " + outputBcFileLinux
    x.StartInfo.CreateNoWindow <- true
    x.StartInfo.WindowStyle <- System.Diagnostics.ProcessWindowStyle.Hidden
    x.Start () |> ignore
    x.WaitForExit()
    x.Dispose()
    let output = File.ReadAllText outputFile
    test <@ output = expectedOutput @>
    File.Delete outputFile
    File.Delete tempFile
    File.Delete outputLlvmFile
    File.Delete outputBcFile
