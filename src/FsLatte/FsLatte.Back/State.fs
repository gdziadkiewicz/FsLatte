namespace FsLatte.Back

 module internal State =
    open System.Collections.Generic
    open FsLatte.Model.IntermediateForm
    open FsLatte.Model.Abs
    type T = 
        {
            mutable localCounter:int
            mutable globalCounter:int
            stringLiteralsMap:Dictionary<string, Value>
            vars:Dictionary<string, Type*Value> Stack
        }
    let empty (): T = {localCounter = 1; globalCounter = 0;  stringLiteralsMap = new Dictionary<string, Value>(); vars = new Stack<Dictionary<string, Type*Value>>()}
    let resetLocalCounter state = state.localCounter <- 1
    let addFunction (state:T) ((name:string), ((Fun _) as functionType)) =
        state.vars.Peek().[name] <- (functionType, Global name)

    let addFunctionDeclaration (state:T) (FnDef(retType,(Ident name),args,_)) =
        let argsTypeList = args |> List.map (fun (Arg (argType,_)) -> argType)
        let functionType = Fun (retType, argsTypeList)
        addFunction state (name, functionType)

    let updateVar (state:T) (Ident s) (value:Value) =
        if state.vars.Peek().ContainsKey s then
            let t = fst (state.vars.Peek().[s])
            state.vars.Peek().[s] <- (t, value)
        else
            failwith ""

    let addVar (state:T) (t:Type) (Ident s) (value:Value) =
            state.vars.Peek().Add(s, (t, value))

    let getVar (state:T) (Ident s) =
            state.vars
            |> Seq.choose (fun layer -> FSharpx.Option.fromTryPattern(layer.TryGetValue) s)
            |> Seq.tryHead

    let getType (state:T) ident : Type option =
            getVar state ident
            |> Option.map fst

    let pushFrame (state:T) =
        state.vars.Push (Dictionary())

    let popFrame (state:T) =
        state.vars.Pop() |> ignore

    let takeNextLocalName state =
        state.localCounter <- state.localCounter + 1
        state.localCounter - 1 |> string |> Register

    let takeNextGlobalName state =
        state.globalCounter <- state.globalCounter + 1
        state.globalCounter - 1 |> string |> Global

    let getAddStringLiteral state s =
        if state.stringLiteralsMap.ContainsKey s then
            state.stringLiteralsMap.[s]
        else
            let newLiteralName = takeNextGlobalName state
            state.stringLiteralsMap.[s] <-  newLiteralName
            newLiteralName