namespace FsLatte.Semantics
open FSharpx
open FsLatte.Model.Abs

module internal ReturnChecker =
    let private hasReturns (body:Block) : bool =
        let rec transStmt (stmt:Stmt) : bool =
            match stmt with
            | Ret _ -> true
            | VRet  -> true
            | BStmt (Block stmts) -> stmts |> List.map transStmt |> List.fold (||) false
            | CondElse (_, stmt0, stmt) -> transStmt stmt0 && transStmt stmt
            | While (_, stmt) -> false
            | Cond (_, stmt) -> false
            | SExp _ -> false
            | Decl _ -> false
            | Ass _ -> false
            | Incr _ -> false
            | Decr _ -> false
            | Empty  -> false

        let (Block stmts) = body
        stmts
        |> List.map transStmt
        |> List.fold (||) false

    let checkIfFunctionsHaveReturns (program:Program) : string list =
        let checkFunction (type':Type) (name:string) (body) : string list =
            let expectedReturnType = type'
            let hasRet = body |> hasReturns
            if hasRet || expectedReturnType = Void then
                []
            else
                [ sprintf "Not all code paths in function %s return" name]
        let (Program program) = program
        program |> List.collect (fun (FnDef (a,(Ident b),_,d) )-> checkFunction a b d)