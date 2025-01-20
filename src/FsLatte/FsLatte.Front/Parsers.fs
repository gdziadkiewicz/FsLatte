module FsLatte.Core.Parsers
open FSharpx
open FParsec
open Latte.Abs

//let str s = pstring s
//let integer = pint32
//let program topDef = sepBy1 topDef newline
//let pTopDef type' ident parenthesised args block =
//    tuple4 type' ident (parenthesised args) block
//let ident () = identifier (IdentifierOptions())
//let commaSeparated p = sepBy p (str ",") 
//let args arg = commaSeparated arg
//
//let lparenthesis = str "("
//let rparenthesis = str ")"
//let parenthesised p =  between lparenthesis rparenthesis p
//let lbrace = str "{"
//let rbrace = str "}"
//let braced p = between lbrace rbrace p 
//
//let block braced statments = braced statments
//
//let statments statment = many statment
//let statment statmentTypes = choice statmentTypes
//
//let intType  = (str "int") >>%  Int
//let stringType  = (str "string") >>%  Str
//let boolType  = (str "boolean") >>%  Bool
//let voidType = (str "void") >>%  Void
//let rec type' = choice [intType; stringType; boolType; voidType; funType ()]
//and funType () =
//    pipe2 type' (parenthesised <| commaSeparated type') <| curry Fun

type UserState = unit // doesn't have to be unit, of course
type ParserV<'t> = Parser<'t, UserState>

let str s : ParserV<_> = pstring s
let semicolon = str ";"
let equals = str "="

let lparenthesis = str "("
let rparenthesis = str ")"
let parenthesised p =  between lparenthesis rparenthesis p

let lbrace = str "{"
let rbrace = str "}"
let braced p = between lbrace rbrace p 


let commaSeparated p = sepBy p (str ",")
let commaSeparated1 p = sepBy1 p (str ",")

let integer = pint32

let comment1 = str "#" >>.  skipRestOfLine true
let comment2 = str "//" >>.  skipRestOfLine true
let comment3 = between (str "/*") (str "*/") sk

let pIdent = identifier (IdentifierOptions()) |>> Ident
let pIntType  = (str "int") >>%  Int
let pStringType  = (str "string") >>%  Str
let pBoolType  = (str "boolean") >>%  Bool
let pVoidType = (str "void") >>%  Void
//TODO: zrobić funType (jak to zrobić jest chyba na wykładzie)
//let funType type' = pipe2 type' (parenthesised <| commaSeparated type') <| curry Fun
let pType : ParserV<TypeT> = choice [pIntType; pStringType; pBoolType; pVoidType;] //funType type']

let pExpr: ParserV<Expr> = failwith "A"

let pNoInit = pIdent |>> NoInit
let pInit = pIdent .>> equals .>>. pExpr |>> Init
let pItem = pNoInit <|> pInit

let pEmpty = semicolon >>% Empty
//let pBStmt = block |>> Block
let pDecl = pType .>>. commaSeparated1 pItem .>> semicolon |>> Decl
let pAss = pIdent .>> equals .>>. pExpr .>> semicolon |>> Ass
let pIncr = pIdent .>> str "++" |>> Incr
let pDecr = pIdent .>> str "--" |>> Decr
let pRet = str "return" >>. pExpr .>> semicolon |>> Ret
let pVRet = str "return" >>. semicolon >>% VRet
//let pCond of Expr * Stmt
//let pCondElse of Expr * Stmt * Stmt
//let pWhile of Expr * Stmt
let pSExp = pExpr .>> semicolon |>> SExp
let statmentTypes = [
                    pEmpty
//                    pBStmt
                    pDecl
                    pAss 
                    pIncr
                    pDecr
                    pRet 
                    pVRet
//                    pCond
//                    pCondElse
//                    pWhile
                    pSExp
                    ]
let statment : ParserV<Stmt> = choice statmentTypes
let statments = many statment
let block = braced statments

let arg = pType .>>. pIdent 
let args = commaSeparated arg

let pTopDef = tuple4 pType pIdent (parenthesised args) block
let pProgram = sepBy1 pTopDef newline