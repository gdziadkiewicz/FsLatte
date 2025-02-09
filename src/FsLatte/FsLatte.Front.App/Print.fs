// pretty-printer generated by the BNF converter
module FsLatte.Front.App.Print

open System
open System.Text
open FsLatte.Model.Abs

type Doc = StringBuilder -> int -> int

let rec printTree (printer : int -> 'a -> Doc) (tree : 'a) : string = 
    let initSize = 16
    let buffer = StringBuilder initSize
    printer 0 tree buffer 0 |> ignore
    buffer.ToString()

let indentWidth = 4

let indent (i: int) : string = "\n" + String.replicate i " "
let append (s:string) (sb:StringBuilder) = sb.Append s

// this render function is written for C-style languages, you may want to change it
let render (s : string) : Doc = fun buf i -> 
    let n = buf.Length
    let last = if n = 0 then None else Some (buf.Chars (n-1))
    let whitespace =
        match last with
        | None -> "" 
        | Some '{' -> indent i
        | Some '}' ->
            match s with
            | ";" -> ""
            | _ -> indent i
        | Some ';' -> indent i
        | Some '[' |  Some '(' -> ""
        | Some _ ->
            match s with
            | "," | ")" | "]" -> ""
            | _ -> " "
    let newindent =
        match s with
        | "{" -> i + indentWidth
        | "}" -> i - indentWidth
        | _ -> i
    buf |> append whitespace |> append s |> ignore
    newindent

let emptyDoc : Doc = fun _ i -> i

let concatD (ds : Doc list) : Doc = fun buf i -> 
    List.fold (fun accIndent elemDoc -> elemDoc buf accIndent) (emptyDoc buf i) ds

let parenth (d:Doc) : Doc = concatD [render "("; d; render ")"]

let prPrec (i:int) (j:int) (d:Doc) : Doc = if j<i then parenth d else d


let rec prtchar (_:int) (c:char) : Doc = render ("'" + string c + "'")


let rec prtint (_:int) (i:int) : Doc = render (string i)


let rec prtfloat (_:int) (f:float) : Doc = render (sprintf "%f" f)


let rec prtString (_:int) (s:string) : Doc = render ("\"" + s + "\"")


let rec prtIdent _ (Ident i) : Doc = render i



let rec prtProgram (i:int) (e:Program) : Doc =
    match e with
    | Program topdefs -> prPrec i 0 (concatD [prtTopDefListBNFC 0 topdefs])


and prtTopDef (i:int) (e:TopDef) : Doc =
    match e with
    | FnDef (type_, ident, args, block) -> prPrec i 0 (concatD [prtType 0 type_ ; prtIdent 0 ident ; render "(" ; prtArgListBNFC 0 args ; render ")" ; prtBlock 0 block])

and prtTopDefListBNFC i es : Doc =
    match (i, es) with
    | (_,[x]) -> (concatD [prtTopDef 0 x])
    | (_,x::xs) -> (concatD [prtTopDef 0 x ; prtTopDefListBNFC 0 xs])
and prtArg (i:int) (e:Arg) : Doc =
    match e with
    | Arg (type_, ident) -> prPrec i 0 (concatD [prtType 0 type_ ; prtIdent 0 ident])

and prtArgListBNFC i es : Doc =
    match (i, es) with
    | (_,[]) -> (concatD [])
    | (_,[x]) -> (concatD [prtArg 0 x])
    | (_,x::xs) -> (concatD [prtArg 0 x ; render "," ; prtArgListBNFC 0 xs])
and prtBlock (i:int) (e:Block) : Doc =
    match e with
    | Block stmts -> prPrec i 0 (concatD [render "{" ; prtStmtListBNFC 0 stmts ; render "}"])


and prtStmt (i:int) (e:Stmt) : Doc =
    match e with
    | Empty  -> prPrec i 0 (concatD [render ";"])
    | BStmt block -> prPrec i 0 (concatD [prtBlock 0 block])
    | Decl (type_, items) -> prPrec i 0 (concatD [prtType 0 type_ ; prtItemListBNFC 0 items ; render ";"])
    | Ass (ident, expr) -> prPrec i 0 (concatD [prtIdent 0 ident ; render "=" ; prtExpr 0 expr ; render ";"])
    | Incr ident -> prPrec i 0 (concatD [prtIdent 0 ident ; render "++" ; render ";"])
    | Decr ident -> prPrec i 0 (concatD [prtIdent 0 ident ; render "--" ; render ";"])
    | Ret expr -> prPrec i 0 (concatD [render "return" ; prtExpr 0 expr ; render ";"])
    | VRet  -> prPrec i 0 (concatD [render "return" ; render ";"])
    | Cond (expr, stmt) -> prPrec i 0 (concatD [render "if" ; render "(" ; prtExpr 0 expr ; render ")" ; prtStmt 0 stmt])
    | CondElse (expr, stmt1, stmt2) -> prPrec i 0 (concatD [render "if" ; render "(" ; prtExpr 0 expr ; render ")" ; prtStmt 0 stmt1 ; render "else" ; prtStmt 0 stmt2])
    | While (expr, stmt) -> prPrec i 0 (concatD [render "while" ; render "(" ; prtExpr 0 expr ; render ")" ; prtStmt 0 stmt])
    | SExp expr -> prPrec i 0 (concatD [prtExpr 0 expr ; render ";"])

and prtStmtListBNFC i es : Doc =
    match (i, es) with
    | (_,[]) -> (concatD [])
    | (_,x::xs) -> (concatD [prtStmt 0 x ; prtStmtListBNFC 0 xs])
and prtItem (i:int) (e:Item) : Doc =
    match e with
    | NoInit ident -> prPrec i 0 (concatD [prtIdent 0 ident])
    | Init (ident, expr) -> prPrec i 0 (concatD [prtIdent 0 ident ; render "=" ; prtExpr 0 expr])

and prtItemListBNFC i es : Doc =
    match (i, es) with
    | (_,[x]) -> (concatD [prtItem 0 x])
    | (_,x::xs) -> (concatD [prtItem 0 x ; render "," ; prtItemListBNFC 0 xs])
and prtType (i:int) (e:Type) : Doc =
    match e with
    | Int  -> prPrec i 0 (concatD [render "int"])
    | Str  -> prPrec i 0 (concatD [render "string"])
    | Bool  -> prPrec i 0 (concatD [render "boolean"])
    | Void  -> prPrec i 0 (concatD [render "void"])
    | Fun (type_, types) -> prPrec i 0 (concatD [prtType 0 type_ ; render "(" ; prtTypeListBNFC 0 types ; render ")"])

and prtTypeListBNFC i es : Doc =
    match (i, es) with
    | (_,[]) -> (concatD [])
    | (_,[x]) -> (concatD [prtType 0 x])
    | (_,x::xs) -> (concatD [prtType 0 x ; render "," ; prtTypeListBNFC 0 xs])
and prtExpr (i:int) (e:Expr) : Doc =
    match e with
    | EVar ident -> prPrec i 6 (concatD [prtIdent 0 ident])
    | ELitInt integer -> prPrec i 6 (concatD [prtint 0 integer])
    | ELitTrue  -> prPrec i 6 (concatD [render "true"])
    | ELitFalse  -> prPrec i 6 (concatD [render "false"])
    | EApp (ident, exprs) -> prPrec i 6 (concatD [prtIdent 0 ident ; render "(" ; prtExprListBNFC 0 exprs ; render ")"])
    | EString string -> prPrec i 6 (concatD [prtString 0 string])
    | Neg expr -> prPrec i 5 (concatD [render "-" ; prtExpr 6 expr])
    | Not expr -> prPrec i 5 (concatD [render "!" ; prtExpr 6 expr])
    | EMul (expr1, mulop, expr2) -> prPrec i 4 (concatD [prtExpr 4 expr1 ; prtMulOp 0 mulop ; prtExpr 5 expr2])
    | EAdd (expr1, addop, expr2) -> prPrec i 3 (concatD [prtExpr 3 expr1 ; prtAddOp 0 addop ; prtExpr 4 expr2])
    | ERel (expr1, relop, expr2) -> prPrec i 2 (concatD [prtExpr 2 expr1 ; prtRelOp 0 relop ; prtExpr 3 expr2])
    | EAnd (expr1, expr2) -> prPrec i 1 (concatD [prtExpr 2 expr1 ; render "&&" ; prtExpr 1 expr2])
    | EOr (expr1, expr2) -> prPrec i 0 (concatD [prtExpr 1 expr1 ; render "||" ; prtExpr 0 expr2])

and prtExprListBNFC i es : Doc =
    match (i, es) with
    | (_,[]) -> (concatD [])
    | (_,[x]) -> (concatD [prtExpr 0 x])
    | (_,x::xs) -> (concatD [prtExpr 0 x ; render "," ; prtExprListBNFC 0 xs])
and prtAddOp (i:int) (e:AddOp) : Doc =
    match e with
    | Plus  -> prPrec i 0 (concatD [render "+"])
    | Minus  -> prPrec i 0 (concatD [render "-"])


and prtMulOp (i:int) (e:MulOp) : Doc =
    match e with
    | Times  -> prPrec i 0 (concatD [render "*"])
    | Div  -> prPrec i 0 (concatD [render "/"])
    | Mod  -> prPrec i 0 (concatD [render "%"])


and prtRelOp (i:int) (e:RelOp) : Doc =
    match e with
    | LTH  -> prPrec i 0 (concatD [render "<"])
    | LE  -> prPrec i 0 (concatD [render "<="])
    | GTH  -> prPrec i 0 (concatD [render ">"])
    | GE  -> prPrec i 0 (concatD [render ">="])
    | EQU  -> prPrec i 0 (concatD [render "=="])
    | NE  -> prPrec i 0 (concatD [render "!="])



