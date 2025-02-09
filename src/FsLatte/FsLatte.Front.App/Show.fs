// show functions generated by the BNF converter
module FsLatte.Front.App.Show

open System.Text
open FsLatte.Model.Abs

type Showable = StringBuilder -> unit

let show (s : Showable) : string = 
    let initSize = 16
    let b = StringBuilder initSize
    s b
    b.ToString()

let emptyS : Showable = ignore

let c2s (c:char) : Showable = fun buf -> buf.Append c |> ignore
let s2s (s:string) : Showable = fun buf -> buf.Append s |> ignore

let ( >> ) (s1:Showable) (s2:Showable) : Showable = fun buf ->
    s1 buf
    s2 buf

let showChar (c:char) : Showable = fun buf -> 
    buf.Append ("'" + string c + "'") |> ignore

let showString (s:string) : Showable = fun buf -> 
    buf.Append ("\"" + s + "\"") |> ignore

let showList (showFun : 'a -> Showable) (xs : 'a list) : Showable = fun buf -> 
    let rec f ys =
        match ys with
        | [] -> ()
        | [y] -> showFun y buf
        | y::ys ->
            showFun y buf
            buf.Append "; " |> ignore
            f ys 
    buf.Append '[' |> ignore
    f xs;
    buf.Append ']' |> ignore

let showint (i:int) : Showable = i |> string |> s2s
let showfloat (f:float) : Showable = f |> string |> s2s
let rec showIdent (Ident i) : Showable = s2s "Ident " >> showString i


let rec showProgram (e:Program) : Showable =
    match e with
    | Program topdefs -> s2s "Program" >> c2s ' ' >> c2s '(' >> showList showTopDef topdefs >> c2s ')'


and showTopDef (e:TopDef) : Showable =
    match e with
    | FnDef (type', ident, args, block) -> s2s "FnDef" >> c2s ' ' >> c2s '(' >> showType type'  >> s2s ", " >>  showIdent ident  >> s2s ", " >>  showList showArg args  >> s2s ", " >>  showBlock block >> c2s ')'


and showArg (e:Arg) : Showable =
    match e with
    | Arg (type', ident) -> s2s "Arg" >> c2s ' ' >> c2s '(' >> showType type'  >> s2s ", " >>  showIdent ident >> c2s ')'


and showBlock (e:Block) : Showable =
    match e with
    | Block stmts -> s2s "Block" >> c2s ' ' >> c2s '(' >> showList showStmt stmts >> c2s ')'


and showStmt (e:Stmt) : Showable =
    match e with
    | Empty  -> s2s "Empty" 
    | BStmt block -> s2s "BStmt" >> c2s ' ' >> c2s '(' >> showBlock block >> c2s ')'
    | Decl (type', items) -> s2s "Decl" >> c2s ' ' >> c2s '(' >> showType type'  >> s2s ", " >>  showList showItem items >> c2s ')'
    | Ass (ident, expr) -> s2s "Ass" >> c2s ' ' >> c2s '(' >> showIdent ident  >> s2s ", " >>  showExpr expr >> c2s ')'
    | Incr ident -> s2s "Incr" >> c2s ' ' >> c2s '(' >> showIdent ident >> c2s ')'
    | Decr ident -> s2s "Decr" >> c2s ' ' >> c2s '(' >> showIdent ident >> c2s ')'
    | Ret expr -> s2s "Ret" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
    | VRet  -> s2s "VRet" 
    | Cond (expr, stmt) -> s2s "Cond" >> c2s ' ' >> c2s '(' >> showExpr expr  >> s2s ", " >>  showStmt stmt >> c2s ')'
    | CondElse (expr, stmt0, stmt) -> s2s "CondElse" >> c2s ' ' >> c2s '(' >> showExpr expr  >> s2s ", " >>  showStmt stmt0  >> s2s ", " >>  showStmt stmt >> c2s ')'
    | While (expr, stmt) -> s2s "While" >> c2s ' ' >> c2s '(' >> showExpr expr  >> s2s ", " >>  showStmt stmt >> c2s ')'
    | SExp expr -> s2s "SExp" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'


and showItem (e:Item) : Showable =
    match e with
    | NoInit ident -> s2s "NoInit" >> c2s ' ' >> c2s '(' >> showIdent ident >> c2s ')'
    | Init (ident, expr) -> s2s "Init" >> c2s ' ' >> c2s '(' >> showIdent ident  >> s2s ", " >>  showExpr expr >> c2s ')'


and showType (e:Type) : Showable =
    match e with
    | Int  -> s2s "Int" 
    | Str  -> s2s "Str" 
    | Bool  -> s2s "Bool" 
    | Void  -> s2s "Void" 
    | Fun (type', types) -> s2s "Fun" >> c2s ' ' >> c2s '(' >> showType type'  >> s2s ", " >>  showList showType types >> c2s ')'


and showExpr (e:Expr) : Showable =
    match e with
    | EVar ident -> s2s "EVar" >> c2s ' ' >> c2s '(' >> showIdent ident >> c2s ')'
    | ELitInt integer -> s2s "ELitInt" >> c2s ' ' >> c2s '(' >> showint integer >> c2s ')'
    | ELitTrue  -> s2s "ELitTrue" 
    | ELitFalse  -> s2s "ELitFalse" 
    | EApp (ident, exprs) -> s2s "EApp" >> c2s ' ' >> c2s '(' >> showIdent ident  >> s2s ", " >>  showList showExpr exprs >> c2s ')'
    | EString string -> s2s "EString" >> c2s ' ' >> c2s '(' >> showString string >> c2s ')'
    | Neg expr -> s2s "Neg" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
    | Not expr -> s2s "Not" >> c2s ' ' >> c2s '(' >> showExpr expr >> c2s ')'
    | EMul (expr0, mulop, expr) -> s2s "EMul" >> c2s ' ' >> c2s '(' >> showExpr expr0  >> s2s ", " >>  showMulOp mulop  >> s2s ", " >>  showExpr expr >> c2s ')'
    | EAdd (expr0, addop, expr) -> s2s "EAdd" >> c2s ' ' >> c2s '(' >> showExpr expr0  >> s2s ", " >>  showAddOp addop  >> s2s ", " >>  showExpr expr >> c2s ')'
    | ERel (expr0, relop, expr) -> s2s "ERel" >> c2s ' ' >> c2s '(' >> showExpr expr0  >> s2s ", " >>  showRelOp relop  >> s2s ", " >>  showExpr expr >> c2s ')'
    | EAnd (expr0, expr) -> s2s "EAnd" >> c2s ' ' >> c2s '(' >> showExpr expr0  >> s2s ", " >>  showExpr expr >> c2s ')'
    | EOr (expr0, expr) -> s2s "EOr" >> c2s ' ' >> c2s '(' >> showExpr expr0  >> s2s ", " >>  showExpr expr >> c2s ')'


and showAddOp (e:AddOp) : Showable =
    match e with
    | Plus  -> s2s "Plus" 
    | Minus  -> s2s "Minus" 


and showMulOp (e:MulOp) : Showable =
    match e with
    | Times  -> s2s "Times" 
    | Div  -> s2s "Div" 
    | Mod  -> s2s "Mod" 


and showRelOp (e:RelOp) : Showable =
    match e with
    | LTH  -> s2s "LTH" 
    | LE  -> s2s "LE" 
    | GTH  -> s2s "GTH" 
    | GE  -> s2s "GE" 
    | EQU  -> s2s "EQU" 
    | NE  -> s2s "NE" 



