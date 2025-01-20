namespace FsLatte.Semantics
open Chessie.ErrorHandling
open FsLatte.Model.Abs
open System.Collections.Generic


module internal TypeChecker =
    module private State =
        type T = Dictionary<string,Type> Stack
        let empty (): T = Stack()
        

        let addFunction (state:T) ((name:string), ((Fun _) as functionType)) =
            state.Peek().[name] <- functionType

        let addFunctionDeclaration (state:T) (FnDef(retType,(Ident name),args,_)) =
            let argsTypeList = args |> List.map (fun (Arg (argType,_)) -> argType)
            let functionType = Fun (retType, argsTypeList)
            addFunction state (name, functionType)

        let addVar (state:T) (t:Type) (Ident s) =
            if not (state.Peek().ContainsKey s) then
                state.Peek().Add (s, t)
        let getType (state:T) (Ident s) : Type option =
             state
             |> Seq.choose (fun layer -> FSharpx.Option.fromTryPattern(layer.TryGetValue) s)
             |> Seq.tryHead

        let addExpectedReturn (state: T) type' =
            state.Peek().Add("@return", type')

        let getExpectedReturnType (state: T) =
            getType state (Ident "@return") |> Option.get

        let pushFrame (state:T) =
            state.Push (Dictionary())

        let popFrame (state:T) =
            state.Pop() |> ignore

    type Result = string list

    let failure x = failwith "Undefined case."

    let public checkTypes (prog:Program) : string list =
        let state = State.empty()
        State.pushFrame state
        let rec transIdent (Ident x) : string = x

        and transProgram (Program topdefs) : Result =
            FsLatte.Model.PredefinedFunctions.functionsList |> List.iter ( State.addFunction state )
            topdefs |> List.iter (State.addFunctionDeclaration state )
            topdefs |> List.collect transTopDef

        and transTopDef (FnDef (type', ident, args, block)) : Result =
            State.pushFrame state
            State.addExpectedReturn state type'
            args |> List.iter (fun (Arg(t,i)) -> State.addVar state t i)
            let results = block |> transBlock
            State.popFrame state
            results

        and transBlock (Block stmts) : Result =
            State.pushFrame state
            let results = stmts |> List.collect transStmt
            State.popFrame state
            results

        and transStmt (x : Stmt) : Result =
            match x with
            | Empty  -> []
            | BStmt block -> block |> transBlock
            | Decl (t, items) ->
                items |> List.map (function |Init(ident, _)|NoInit(ident) -> ident) |> List.iter (State.addVar state t)
                items |> List.collect transItem
            | Ass (ident, expr) ->
                let (r, expressionType) = transExpr expr
                match State.getType state ident with
                | Some expectedType when expectedType = expressionType -> r
                | Some expectedType -> sprintf "Variable %s has type %s and is assigned with expression of type %s" (transIdent ident) (transType expectedType) (transType expressionType) :: r
                | None -> sprintf "Variable %s is not declared" (transIdent ident) :: r
            | Incr ident | Decr ident ->
                match State.getType state ident with
                | Some Int -> []
                | Some t -> [sprintf "Variable %s has type %s. ++/-- operators can only be used on int type vars." (transIdent ident) (transType t) ]
                | None -> []
            | Ret expr ->
                let (r, expressionType) = transExpr expr
                let expectedReturnType = State.getExpectedReturnType state
                if expectedReturnType <> Void && expectedReturnType = expressionType then
                    r
                elif expectedReturnType = Void then
                    "Expression return from void function is not allowed." :: r
                else
                    sprintf "Function return type mismatch. Expected: %s Actual: %s" (transType expectedReturnType) (transType expressionType) :: r
            | VRet  ->
                match State.getExpectedReturnType state with
                | Void -> []
                | x -> sprintf "Void return from function with return type %s is not allowed" (transType x) |>List.singleton
            | Cond (expr, stmt) ->
                let (r, expressionType) = transExpr expr
                let typeError =
                    if expressionType <> Bool then
                        sprintf "If condition expression must be of type bool. Here: %s" (transType expressionType) |> List.singleton
                    else
                        []
                State.pushFrame state
                let results = stmt |> transStmt
                State.popFrame state
                r @ typeError @ results
            | CondElse (expr, stmt0, stmt) ->
                let (r, expressionType) = transExpr expr
                let typeError =
                    if expressionType <> Bool then
                        sprintf "If condition expression must be of type bool. Here: %s" (transType expressionType) |> List.singleton
                    else
                        []
                State.pushFrame state
                let results0 = stmt0 |> transStmt
                State.popFrame state
                State.pushFrame state
                let results = stmt |> transStmt
                State.popFrame state
                r @ typeError @ results0 @ results
            | While (expr, stmt) ->
                let (r, expressionType) = transExpr expr
                let typeError =
                    if expressionType <> Bool then
                        sprintf "While condition expression must be of type bool. Here: %s" (transType expressionType) |> List.singleton
                    else
                        []
                State.pushFrame state
                let results = stmt |> transStmt
                State.popFrame state
                r @ typeError @ results
            | SExp expr -> expr |> transExpr |> fst

        and transItem (x : Item) : Result =
            match x with
            | NoInit _ -> []
            | Init (ident, expr) ->
                let (Some expectedType) = State.getType state ident
                let (r,t) = transExpr expr
                if t <> expectedType then
                    sprintf "Variable %s has type %s and is initialized with expression of type %s" (transIdent ident) (transType expectedType) (transType t) ::r
                else
                    r

        and transType (x : Type) : string =
            match x with
            | Int  -> "int"
            | Str  -> "string"
            | Bool  -> "bool"
            | Void  -> "void"
            | Fun (type', types) -> failure x

        and transExpr (x : Expr) : Result*Type =
            match x with
            | EVar ident ->
                match State.getType state ident with
                | Some t -> ([],t)
                | None -> ([sprintf "Use of undeclared variable %s." (transIdent ident)], Type.Void)
            | ELitInt _ -> ([], Type.Int)
            | ELitTrue | ELitFalse  -> ([], Type.Bool)
            | EString _ -> ([], Type.Str)
            | EApp (ident, exprs) ->
                let transArgs xs = xs |> List.map transType |> FSharpx.Strings.separatedBy ", "
                let x = exprs |> List.map transExpr
                let argTypes = x |> List.map snd
                let errors = x |> List.collect fst

                match State.getType state ident with
                | None -> (sprintf "Use of undeclared function %s" (transIdent ident) :: errors, Type.Void)
                | Some (Fun (rt, paramTypes)) when argTypes = paramTypes -> (errors, rt)
                | Some (Fun(rt, pts)) -> (sprintf "(%s) function called with (%s)" (transArgs pts) (transArgs argTypes) ::errors, rt)
                | Some t -> (sprintf "Type %s used like function." (transType t) :: errors, Type.Void)
            | Neg expr ->
                let (r, expressionType) = transExpr expr
                match expressionType with
                | Int -> (r, Type.Int)
                | ot -> ("Unary (-) can only be used with int." :: r, ot)
            | Not expr ->
                let (r, expressionType) = transExpr expr
                match expressionType with
                | Bool -> (r, Type.Bool)
                | ot -> ("Negation operator can only be used with bool." :: r, ot)
            | EMul (expr0, mulop, expr) ->
                let (r0, et0) = transExpr expr0
                let (r, et) = transExpr expr
                if et0 <> Int || et <> Int then
                    (sprintf "Both (*)/(/) args must be of Int type." :: r0 @ r ,Int)
                else
                    (r0@r, Int)
            | EAdd (expr0, addop, expr) ->
                match addop with
                | Plus  -> 
                    let (r0, et0) = transExpr expr0
                    let (r, et) = transExpr expr
                    let t = if et0 = Int || et0 = Str then et0 elif et = Str then Str else Int
                    if et0 = et && (et = Int || et = Str)  then
                        (r0@r, et)
                    else
                        (sprintf "Both (+) args must be of type %s." (transType t) :: r0 @ r ,t)
                | Minus  ->
                    let (r0, et0) = transExpr expr0
                    let (r, et) = transExpr expr
                    if et0 <> Int || et <> Int then
                        (sprintf "Both (-) args must be of type int." :: r0 @ r ,Int)
                    else
                        (r0@r, Int)
            | ERel (expr0, relop, expr1) ->
                let (r0, et0) = transExpr expr0
                let (r, et1) = transExpr expr1
                let (|EqOrNe|_|) (rop) = if rop = EQU || rop = NE then Some () else None
                match (relop,et0,et1) with
                | (EqOrNe, Int, Int) | (EqOrNe, Bool, Bool) | (_, Int, Int) -> (r0@r, Bool)
                | (EqOrNe, _, _) -> (sprintf "Both equality/inequality operator args must be of type int or bool." :: r0 @ r, Bool)
                | _ -> (sprintf "Both relation operator args must be of type int." :: r0 @ r, Bool)

            | EAnd (expr0, expr) | EOr (expr0, expr) ->
                let (r0, et0) = transExpr expr0
                let (r, et) = transExpr expr
                if et0 <> Bool || et <> Bool then
                    (sprintf "Both and/or operator args must be of type bool." :: r0 @ r, Bool)
                else
                    (r0@r, Bool)

        prog |> transProgram

