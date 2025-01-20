namespace FsLatte.Semantics
open FSharpx
open FsLatte.Model.Abs
open System.Collections.Generic
module internal FunctionChecker =
    let checkFunctionsRedefinition (program:Program) : string list =
        let functions = FunctionHelper.getFunctionFromProgram program
        let functionsGroupedByName = functions |> Seq.ofList |> Seq.groupBy (fun (_,k,_) -> k)
        let functionNamesWithMultipleDefinition = functionsGroupedByName |> Seq.filter (snd>>Seq.length>>(<) 1)
        functionNamesWithMultipleDefinition |> Seq.map (fst >> sprintf "Function %s has multiple definitions.") |> List.ofSeq
    
    let checkFunctionsArgs (program:Program) : string list =
        let f (_, name: string, args: Arg list) =
            let duplicateNameArgs = args |> Seq.ofList |> Seq.groupBy (fun (Arg (_,(Ident idnent))) -> idnent) |> Seq.filter (snd>>Seq.length>>(<) 1) |> Seq.map fst
            if Seq.isEmpty duplicateNameArgs then
                None
            else
                Some (name, duplicateNameArgs)
        let functions = FunctionHelper.getFunctionFromProgram program
        let faultyArgNamesWithFunctions = functions |> List.choose f
        faultyArgNamesWithFunctions |> List.collect (fun (x,xs) -> xs |> Seq.map (sprintf "Function %s has multiple parameters with identifier %s" x) |> Seq.toList)
    
    let checkVariablesRedefinitions (program:Program) : string list =
        let checkFunction (name, (args:Arg list), (Block body)) : string list =
            let vars : Stack<HashSet<string>> = Stack()
            vars.Push (HashSet())
            args |> List.map (fun (Arg (x, (Ident s))) -> s) |> List.iter (vars.Peek().Add >> ignore)
            let rec transStmt (stmt:Stmt) : string list =
                let analyzeInNewFrame stmts =
                    vars.Push (HashSet())
                    let errors =  stmts |> List.collect transStmt
                    vars.Pop() |> ignore
                    errors 
                match stmt with
                | BStmt (Block stmts) -> analyzeInNewFrame stmts 
                | CondElse (_, stmt0, stmt) ->[stmt0; stmt] |> List.map List.singleton |> List.collect analyzeInNewFrame
                | While (_, stmt) -> analyzeInNewFrame [stmt]
                | Cond (_, stmt) -> analyzeInNewFrame [stmt]
                | Decl (_, xs) ->
                    let varNames = xs |> List.map (function |NoInit (Ident s) | Init ((Ident s), _) -> s)
                    let result = varNames |> List.map (fun s -> s, (vars.Peek().Add s))
                    result |>List.filter (snd>>not) |> List.map (fun (s,_) -> sprintf "Variable %s redefinition in function %s" s name)
                | _ -> []
            body |> List.collect transStmt

        let functions = program |> (fun (Program xs) -> xs) |> List.map (fun (FnDef (_, (Ident name), args, body)) -> (name, args, body))
        functions |> List.collect checkFunction

