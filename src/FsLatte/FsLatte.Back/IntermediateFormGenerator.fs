namespace FsLatte.Back
open Chessie.ErrorHandling
open FsLatte.Model.IntermediateForm
open FsLatte.Model.Abs

module public IntermediateFormGenerator =
    exception PiesekException of string
    
    module private Implementation =
        let failure x =  PiesekException (x.ToString()) |> raise

        let getTypeDefaultValue =
                function
                | Bool -> Literal "0"
                | Int -> Literal "0"
                | x -> failwith  (sprintf "No default value for type %A" x)

        let toLlvmType (type':Type) : LlvmType =
                match type' with
                | Int  -> I32
                | Str  ->  I8 |> Ptr
                | Bool  -> I1
                | Void  -> LlvmType.VoidT
                | x -> failwith  (sprintf "No llvm type for type %A" x)

        let makeVoidRet = LlvmStmt  (Instruction.Ret, LlvmType.VoidT, [])
        let makeTypedRet value type' = LlvmStmt (Instruction.Ret, type', [(type', value)])
        let transIdent (Ident s) : string = s
        let transType (t : Type) : LlvmType = t |> toLlvmType 

        let fixEmptyVoidBlock t block =
            match (t, block) with
            | (Void, Block stmts) when stmts |> List.tryLast <> Some VRet -> stmts@[VRet] |> Block
            | _ -> block
        let fixLastBlockWithoutRet t lines =
            let t' = toLlvmType t
            let lastLine = lines |> List.last
            match lastLine with
            | Label _ -> lines @ [makeTypedRet (getTypeDefaultValue t) t' ]
            | _ -> lines

        let transRelOp =
                function
                | LTH  -> "slt"
                | LE  -> "sle"
                | GTH  -> "sgt"
                | GE  -> "sge"
                | EQU  -> "eq"
                | NE  -> "ne"

        let doWork program =
            let state = State.empty()
            State.pushFrame state
            FsLatte.Model.PredefinedFunctions.functionsList |> List.iter ( State.addFunction state )

            let addArg ((llvmType, value), Arg(type', ident)) =
                let newLoc = State.takeNextLocalName state
                State.addVar state type' ident newLoc
                [
                LlvmAssignment(newLoc, Alloca, llvmType |> Ptr,[(llvmType, Literal "")])
                LlvmStmt(Store, VoidT, [(llvmType, value); (llvmType |> Ptr, newLoc)])
                ]
            let addArgs args =
                args |> List.collect addArg
            let rec transProgram (Program topdefs) : Function list =
                topdefs
                |> List.map
                    (fun (FnDef (type',(Ident ident), args, _)) ->
                        let args = args |> List.map (fun (Arg(t,_)) -> t)
                        (ident, Fun (type', args))
                    )
                |> List.iter (State.addFunction state)
                topdefs |> List.map (fun td ->
                    State.resetLocalCounter state
                    transTopDef td)

            and transTopDef (topDef : TopDef) : Function =
                match topDef with
                | FnDef (type', ident, args, block) ->
                    let name = transIdent ident |> Global
                    let returnType = transType type'
                    State.pushFrame state
                    let llvmArgs = args |> List.map transArg
                    let arrity : Arrity = (returnType, llvmArgs |> List.map fst)
                    let lines = llvmArgs |> addArgs
                    let body =
                        block
                        |> fixEmptyVoidBlock type'
                        |> transBlock
                        |> fixLastBlockWithoutRet type'
                    State.popFrame state
                    (name, arrity, lines @ body)

            and transArg ((Arg (type', _)) as arg) =
                let name = State.takeNextLocalName state
                let llvmType = transType type'
                ((llvmType, name), arg)

            and transBlock (Block stmts) : LlvmLine list =
                State.pushFrame state
                let r = stmts |> List.collect transStmt
                State.popFrame state
                r

            and transStmt (stmt : Stmt) : LlvmLine list =
                match stmt with
                | Empty  -> []
                | BStmt block -> transBlock block
                | Decl (type', items) -> items |> List.collect (transItem type')
                | Ass (ident, expr) ->
                    let (lines, v, t) = transExpr expr
                    let (_, loc) = State.getVar state ident |> Option.get
                    let s = LlvmStmt(Store, LlvmType.VoidT, [(t, v); (t|>Ptr, loc)])
                    lines@[s]
                | Incr ident ->
                    let newStmt = Ass (ident, EAdd(EVar ident, Plus, ELitInt 1))
                    transStmt newStmt
                | Decr ident ->
                    let newStmt = Ass (ident, EAdd(EVar ident, Minus, ELitInt 1))
                    transStmt newStmt
                | Ret expr ->
                    let (exprStmts, loc, type') = transExpr expr
                    let retStmt = makeTypedRet loc type'
                    exprStmts @ [retStmt]
                | VRet -> List.singleton makeVoidRet
                | SExp expr ->
                    let (stmts, _, _) = expr |> transExpr
                    stmts

                | Cond (expr, stmt) ->
                    let (comparisionLines, comparisionResult, t) = transExpr expr
                    let trueLabel = State.takeNextLocalName state
                    let falseLabel = State.takeNextLocalName state
                    let ifBr = LlvmStmt(Br, VoidT, [(t, comparisionResult); (LabelT, trueLabel); (LabelT, falseLabel)])
                    let trueLabelLine = Label trueLabel
                    let falseLabelLine = Label falseLabel
                    State.pushFrame state
                    let trueLines = transStmt stmt  
                    State.popFrame state
                    let mandatoryBr = LlvmStmt(Br, VoidT, [(LabelT, falseLabel)])
                    comparisionLines @
                    [
                    ifBr
                    trueLabelLine
                    ] @ trueLines @
                    [
                    mandatoryBr
                    falseLabelLine
                    ]
                | CondElse (expr, stmt0, stmt1) ->
                    let (comparisionLines, comparisionResult, t) = transExpr expr
                    let trueLabel = State.takeNextLocalName state
                    let falseLabel = State.takeNextLocalName state
                    let endLabel = State.takeNextLocalName state
                    let ifBr = LlvmStmt(Br, VoidT, [(t, comparisionResult); (LabelT, trueLabel); (LabelT, falseLabel)])
                    let trueLabelLine = Label trueLabel
                    let falseLabelLine = Label falseLabel
                    let endLabelLine = Label endLabel
                    State.pushFrame state
                    let trueLines = transStmt stmt0  
                    State.popFrame state
                    State.pushFrame state
                    let falseLines = transStmt stmt1  
                    State.popFrame state
                    let mandatoryBr = LlvmStmt(Br, VoidT, [(LabelT, endLabel)])
                    comparisionLines @
                    [
                    ifBr
                    trueLabelLine
                    ] @ trueLines @
                    [
                    mandatoryBr
                    falseLabelLine
                    ] @ falseLines @
                    [
                      mandatoryBr
                      endLabelLine
                    ]

                | While (expr, stmt) ->
                    let (comparisionLines, comparisionResult, t) = transExpr expr
                    let conditionLabel = State.takeNextLocalName state
                    let trueLabel = State.takeNextLocalName state
                    let falseLabel = State.takeNextLocalName state
                    let ifBr = LlvmStmt(Br, VoidT, [(t, comparisionResult); (LabelT, trueLabel); (LabelT, falseLabel)])
                    let conditionLabelLine = Label conditionLabel
                    let trueLabelLine = Label trueLabel
                    let falseLabelLine = Label falseLabel
                    State.pushFrame state
                    let trueLines = transStmt stmt  
                    State.popFrame state
                    let backToConditionBrAndMandatoryBr = LlvmStmt(Br, VoidT, [(LabelT, conditionLabel)])
                    [
                    backToConditionBrAndMandatoryBr
                    conditionLabelLine
                    ] @ comparisionLines @
                    [
                    ifBr
                    trueLabelLine
                    ] @ trueLines @
                    [
                    backToConditionBrAndMandatoryBr
                    falseLabelLine
                    ]


            //TODO: Refator to reduce code length
            and transItem t (x : Item) : LlvmLine list =
                match x with
                | NoInit ident ->
                    match t with
                    | Str ->
                        let (llines,v,_) = EString "" |> transExpr
                        let newLoc = State.takeNextLocalName state
                        let a = LlvmAssignment(newLoc, Alloca, I8|>Ptr|>Ptr, [(I8|>Ptr,Literal "")])
                        let s = LlvmStmt(Store, VoidT, [(I8|>Ptr, v); (I8|>Ptr|>Ptr, newLoc)])
                        State.addVar state t ident newLoc
                        llines@[a; s]
                    | _ ->
                        let v = getTypeDefaultValue t
                        let newLoc = State.takeNextLocalName state
                        let alloca = LlvmAssignment(newLoc, Alloca, transType t |> Ptr, [(transType t,Literal "")])
                        let store = LlvmStmt(Store, VoidT, [(transType t, v); (transType t |> Ptr, newLoc)])
                        State.addVar state t ident newLoc
                        [alloca; store]
                | Init (ident, expr) ->
                    let (lines,v,_) = transExpr expr
                    let newLoc = State.takeNextLocalName state
                    let alloca = LlvmAssignment(newLoc, Alloca, transType t |> Ptr, [(transType t,Literal "")])
                    let store = LlvmStmt(Store, VoidT, [(transType t, v); (transType t |> Ptr, newLoc)])
                    State.addVar state t ident newLoc
                    lines @ [alloca; store]

            and transExpr (x : Expr) : LlvmLine list * Value * LlvmType =
                match x with
                | EVar ident ->
                    let (t, value) = State.getVar state ident |> Option.get
                    match t with
                    |  _ ->
                        let newLoc = State.takeNextLocalName state
                        let load = LlvmAssignment(newLoc, Load, transType t, [(transType t |> Ptr, value)])
                        ([load], newLoc, transType t)
                    
                | ELitInt i ->  ([], i|>string|>Value.Literal, I32 )
                | ELitTrue  ->  ([], Value.Literal "true", I1)
                | ELitFalse  -> ([], Value.Literal "false", I1)
                | EApp (ident, exprs) ->
                    let (Fun (rt, _), funName) = State.getVar state ident |> Option.get
                    let rtl = rt |> toLlvmType
                    let argsExpression = exprs |> List.map transExpr
                    let stmts = argsExpression |> List.collect (fun (stmts,_,_) -> stmts)
                    let args = argsExpression |> List.map (fun (_,x,y) -> (y,x))
                    let funName = (rtl, funName)
                    let newLoc = State.takeNextLocalName state
                    let call =
                        match rt with
                        | Void -> LlvmStmt (Call, rtl, funName::args)
                        | _ -> LlvmAssignment (newLoc, Call, rtl, funName::args)
                    (stmts @ [call], newLoc, rtl)
                | EString string ->
                    let newLoc = State.takeNextLocalName state
                    let strPtr = State.getAddStringLiteral state string
                    let arrayType = LlvmType.Array (I8, String.length string + 1)
                    let ass = LlvmAssignment (newLoc, Getelementptr, I8|>Ptr, [ (arrayType |> Ptr, strPtr); (I32, Literal "0"); (I32, Literal "0") ])
                    ([ass], newLoc, I8|>Ptr)
                | Neg expr ->
                    let (exprStmts, loc, type') = transExpr expr
                    let newLoc = State.takeNextLocalName state
                    let newLlvmStmt = LlvmAssignment (newLoc, Sub, I32, [(I32, Literal "0"); (type', loc)] )
                    (exprStmts@[newLlvmStmt], newLoc, I32 )
                | Not expr ->
                    let (exprStmts, loc, type') = transExpr expr
                    let newLoc = State.takeNextLocalName state
                    let newLlvmStmt = LlvmAssignment (newLoc, Xor, I1, [(I1, Literal "true"); (type', loc)] )
                    (exprStmts@[newLlvmStmt], newLoc, I1 )
                | ERel (expr0, relop, expr) ->
                    let (stmts1, v1, t1) as x0 = transExpr expr0
                    let (stmts2, v2, t2) as x1 = transExpr expr
                    let newLoc = State.takeNextLocalName state
                    let style = transRelOp relop
                    let compare = LlvmAssignment(newLoc, Icmp, I1, [(VoidT, Literal style);(t1,v1); (t2,v2)])
                    (stmts1 @ stmts2 @ [compare], newLoc, I1)
                | EMul (expr0, mulop, expr) ->
                    let (stmts1, _, _) as x0 = transExpr expr0
                    let (stmts2, _, _) as x1 = transExpr expr
                    let (stmts3, value, type') = transMulOp mulop x0 x1
                    (stmts1 @ stmts2 @ stmts3, value, type')
                | EAdd (expr0, addop, expr) ->
                    let (stmts1, _, _) as x0 = transExpr expr0
                    let (stmts2, _, _) as x1 = transExpr expr
                    let (stmts3, value, type') = transAddOp addop x0 x1
                    (stmts1 @ stmts2 @ stmts3, value, type')
                | EAnd (expr0, expr1) ->
                    let (stmts1, v1, t1) = transExpr expr0
                    let (stmts2, v2, t2) = transExpr expr1

                    let trueLabel = State.takeNextLocalName state
                    let trueLabelLine = Label trueLabel
                    let nextTestLabel = State.takeNextLocalName state
                    let nextTestLabelLine = Label nextTestLabel
                    let falseLabel = State.takeNextLocalName state
                    let falseLabelLine = Label falseLabel
                    let endLabel = State.takeNextLocalName state
                    let endLabelLine = Label endLabel

                    let ifBr = LlvmStmt(Br, VoidT, [(t1, v1); (LabelT, nextTestLabel); (LabelT, falseLabel)])
                    let ifBr2 = LlvmStmt(Br, VoidT, [(t2, v2); (LabelT, trueLabel); (LabelT, falseLabel)])
                    let toEndBr = LlvmStmt(Br, VoidT, [(LabelT, endLabel)])
                    let newLoc = State.takeNextLocalName state
                    let resultLine = LlvmAssignment(newLoc, Phi, I1, [ (VoidT, Pair(Literal "true", trueLabel)); (VoidT, Pair(Literal "false", falseLabel))])

                    (
                    stmts1 @
                    [
                    ifBr
                    nextTestLabelLine
                    ] @ stmts2 @
                    [
                    ifBr2
                    falseLabelLine
                    toEndBr
                    trueLabelLine
                    toEndBr
                    endLabelLine
                    resultLine
                    ], newLoc, I1)
                | EOr (expr0, expr1) ->
                    let (stmts1, v1, t1) = transExpr expr0
                    let (stmts2, v2, t2) = transExpr expr1

                    let trueLabel = State.takeNextLocalName state
                    let trueLabelLine = Label trueLabel
                    let nextTestLabel = State.takeNextLocalName state
                    let nextTestLabelLine = Label nextTestLabel
                    let falseLabel = State.takeNextLocalName state
                    let falseLabelLine = Label falseLabel
                    let endLabel = State.takeNextLocalName state
                    let endLabelLine = Label endLabel

                    let ifBr = LlvmStmt(Br, VoidT, [(t1, v1); (LabelT, trueLabel); (LabelT, nextTestLabel)])
                    let ifBr2 = LlvmStmt(Br, VoidT, [(t2, v2); (LabelT, trueLabel); (LabelT, falseLabel)])
                    let toEndBr = LlvmStmt(Br, VoidT, [(LabelT, endLabel)])
                    let newLoc = State.takeNextLocalName state
                    let resultLine = LlvmAssignment(newLoc, Phi, I1, [ (VoidT, Pair(Literal "true", trueLabel)); (VoidT, Pair(Literal "false", falseLabel))])

                    (
                    stmts1 @
                    [
                    ifBr
                    nextTestLabelLine
                    ] @ stmts2 @
                    [
                    ifBr2
                    falseLabelLine
                    toEndBr
                    trueLabelLine
                    toEndBr
                    endLabelLine
                    resultLine
                    ], newLoc, I1)

            and transAddOp (addOp : AddOp) (_, v1, t1) (_, v2, t2) : LlvmLine list * Value * LlvmType =
                match (addOp, t1, t2) with
                | (Plus, I32, I32) | (Minus, I32, I32)  ->
                    let instruction = if addOp = Plus then Add else Sub
                    let newLoc = State.takeNextLocalName state
                    ([LlvmAssignment(newLoc, instruction, I32, [(t1,v1); (t2,v2)])], newLoc, I32)
                | (Plus, Ptr I8, Ptr I8) ->
                    let newLoc = State.takeNextLocalName state
                    let (Fun (rt, _), funName) = State.getVar state (Ident "concat") |> Option.get
                    let rtl = rt |> toLlvmType
                    let funName = (rtl, funName)
                    ([LlvmAssignment(newLoc, Call, Ptr I8, [funName; (t1,v1); (t2,v2)]) ], newLoc, Ptr I8 )
                | _ -> failwith "Type error"

            and transMulOp (mulOp : MulOp) (_, v1, t1) (_, v2, t2) : LlvmLine list * Value * LlvmType =
                let instruction = match mulOp with | Times -> Mul | Div -> Sdiv | Mod -> Srem
                let newLoc = State.takeNextLocalName state
                ([LlvmAssignment(newLoc, instruction, I32, [(t1,v1); (t2,v2)])], newLoc, I32)

            let functions = transProgram program
            let stringLiterals = state.stringLiteralsMap |> Seq.map (|KeyValue|) |> List.ofSeq
            (functions, stringLiterals)

    type public CompilationResult = Result<Function list * (string*Value) list, string>

    let public generate (program: Program) : CompilationResult =
        try
            program |> Implementation.doWork |> ok
        with
            | e -> e.Message |> fail 