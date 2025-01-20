namespace FsLatte.Optimalization
open FsLatte.Model.Abs

module internal ConstOptimalization =
    type Result = string

    let (|EBool|_|) (expr: Expr) : bool option =
        match expr with
        | ELitTrue -> Some true
        | ELitFalse -> Some false
        | _ -> None

    let computeRelOp (op : RelOp) x y : bool =
        match op with
        | LTH  -> x<y
        | LE  -> x<=y
        | GTH  -> x>y
        | GE  -> x>=y
        | EQU  -> x=y
        | NE  -> x<>y

    let toEBool x =
        if x then ELitTrue else ELitFalse

    let rec transProgram (Program topdefs) : Program =
        Program (topdefs |> List.map transTopDef)

    and transTopDef (FnDef (type', ident, args, block)) : TopDef =
        FnDef (type', ident, args, transBlock block)

    and transBlock (Block stmts) : Block =
        Block (stmts |> List.map transStmt) 

    and transStmt (s : Stmt) : Stmt =
        match s with
        | BStmt block -> BStmt (transBlock block)
        | Decl (type', items) -> Decl(type', items |> List.map transItem)
        | Ass (ident, estmtpr) -> Ass (ident, transExpr estmtpr)
        | Ret estmtpr ->Ret (transExpr estmtpr)
        | Cond (estmtpr, stmt) ->
            match transExpr estmtpr with
            | ELitTrue -> transStmt stmt
            | ELitFalse -> Empty
            | newExp -> Cond (newExp, transStmt stmt)
        | CondElse (estmtpr, stmt0, stmt1) ->
            match transExpr estmtpr with
            | ELitTrue -> transStmt stmt0
            | ELitFalse -> transStmt stmt1
            | newExp -> CondElse (newExp, transStmt stmt0, transStmt stmt1)
        | While (estmtpr, stmt) ->
            match transExpr estmtpr with
            | ELitFalse -> Empty
            | newExp -> While (newExp, transStmt stmt)
        | SExp estmtpr -> SExp (transExpr estmtpr) 
        | _  -> s

    and transItem (item : Item) : Item =
        match item with
        | NoInit ident -> item
        | Init (ident, expr) -> Init (ident, transExpr expr)

    and transExpr (e : Expr) : Expr =
        match e with
        | EApp (ident, exprs) -> EApp (ident, exprs |> List.map transExpr )
        | Neg expr -> 
            match transExpr expr with
            | ELitInt i  -> ELitInt -i
            | y -> Neg y
        | Not expr ->
            match transExpr expr with
            | ELitTrue  -> ELitFalse
            | ELitFalse  -> ELitTrue
            | y -> Not y
        | EMul (expr0, mulop, expr1) ->
            match (transExpr expr0, transExpr expr1) with
            | (ELitInt x, ELitInt y) -> (match mulop with | Times  -> x*y | Div  -> x/y | Mod  -> x%y) |> ELitInt
            | (ex0, ex1) -> EMul (ex0, mulop, ex1)
        | EAdd (expr0, addop, expr1) ->
            match (transExpr expr0, transExpr expr1) with
            | (ELitInt x, ELitInt y) -> (match addop with | Plus  -> x+y | Minus  -> x-y) |> ELitInt
            | (EString x, EString y) when addop = Plus -> x+y |> EString
            | (ex0, ex1) -> EAdd (ex0, addop, ex1)
        | ERel (expr0, relop, expr1) ->
            match (transExpr expr0, transExpr expr1) with
            | (EBool x, EBool y) -> computeRelOp relop x y |> toEBool
            | (ex0, ex1) -> ERel (ex0, relop, ex1)
        | EAnd (expr0, expr1) ->
            match (transExpr expr0, transExpr expr1) with
            | (EBool x, EBool y) -> (x && y) |> toEBool
            | (ex0, ex1) -> EAnd (ex0, ex1)
        | EOr (expr0, expr1) ->
            match (transExpr expr0, transExpr expr1) with
            | (EBool x, EBool y) -> (x || y) |> toEBool
            | (ex0, ex1) -> EOr (ex0, ex1)
        | _ -> e

    let optimalize (prog:Program) : Program =
        transProgram prog