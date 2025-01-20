module FsLatte.Model.IntermediateForm
open FSharpx

type Instruction =
    | Ret
    | And
    | Or
    | Add
    | Sub
    | Call 
    | Getelementptr
    | Mul
    | Srem
    | Sdiv
    | Xor
    | Alloca
    | Store
    | Load
    | Icmp
    | Br
    | Phi

type LlvmType =
    |I8
    |I32
    |I1
    |VoidT
    |LabelT
    |Array of LlvmType*int
    |Ptr of LlvmType

type Value = 
    | Literal of string  // 0
    | Register of string // %
    | Global of string   // @
    | Pair of Value*Value // [x, y]

let rec stringifyValue = function
    | Literal literal -> literal
    | Register register -> "%v" + register
    | Global global' -> "@" + global'
    | Pair (v1, v2) -> sprintf "[%s, %s]" (stringifyValue v1) (stringifyValue v2)

type ReturnType = LlvmType
type LlvmArg = LlvmType * Value 
type Args = LlvmArg list
type Param = LlvmType * Value
type Params = Param list
type LlvmLine =
    |LlvmStmt of Instruction * ReturnType * Params
    |LlvmAssignment of Value*Instruction * ReturnType * Params
    |Label of Value
type Arrity = ReturnType * Args
type FunctionBody = LlvmLine list
type Function =  Value * Arrity *  FunctionBody

let rec stringifyType (lt:LlvmType) : string =
    match lt with
    | I8 -> "i8"
    | I32 -> "i32"
    | I1 -> "i1"
    | VoidT -> "void"
    | LabelT -> "label"
    | Ptr t -> stringifyType t + "*"
    | Array (t,l) -> sprintf "[%i x %s]" l (stringifyType t)

//TODO: Refactor
let rec toCode llvmLine : string =
    match llvmLine with
    | Label (Register name) -> sprintf "v%s:" name
    | LlvmAssignment (v, ins, rt, args) ->
        let expr = LlvmStmt (ins, rt, args) |> toCode 
        sprintf "%s = %s" (stringifyValue v) expr
    | LlvmStmt (ins, rt, args) ->
        let returnTypeStr = stringifyType rt
        let untypedArgsStr = args |> List.map (snd>>stringifyValue) |> Strings.separatedBy ", "
        let typedArgsStr = args |> List.map (fun (t,v) -> sprintf "%s %s" (stringifyType t) (stringifyValue v)) |> Strings.separatedBy ", "
        let t = Lazy.Create (fun () -> args |> Seq.map ( fun (Ptr stripedType, v) -> stringifyType stripedType) |> Seq.head)
        match ins with
        | Ret -> sprintf "ret %s %s" returnTypeStr untypedArgsStr
        | And -> sprintf "and %s %s" returnTypeStr untypedArgsStr
        | Or  -> sprintf "or %s %s" returnTypeStr untypedArgsStr
        | Add  -> sprintf "add %s %s" returnTypeStr untypedArgsStr
        | Sub  -> sprintf "sub %s %s" returnTypeStr untypedArgsStr
        | Mul  -> sprintf "mul %s %s" returnTypeStr untypedArgsStr
        | Srem  -> sprintf "srem %s %s" returnTypeStr untypedArgsStr
        | Sdiv  -> sprintf "sdiv %s %s" returnTypeStr untypedArgsStr
        | Xor  -> sprintf "xor %s %s" returnTypeStr untypedArgsStr
        | Alloca -> sprintf "alloca %s" typedArgsStr
        | Store -> sprintf "store %s" typedArgsStr
        | Load -> sprintf "load %s, %s" t.Value typedArgsStr
        //| Load -> sprintf "load %s" typedArgsStr
        //| Getelementptr -> sprintf "getelementptr %s" typedArgsStr
        | Getelementptr -> sprintf "getelementptr %s, %s" t.Value typedArgsStr
        | Br -> sprintf "br %s" typedArgsStr
        | Phi -> sprintf "phi %s %s" returnTypeStr untypedArgsStr
        | Icmp ->
            let argsType = args |> List.skip 1 |> List.map (fst>>stringifyType) |> List.head
            let style = args |> List.head |> (snd>>stringifyValue)
            let argsStr = args |> List.skip 1 |> List.map (snd>>stringifyValue) |> Strings.separatedBy ", "
            sprintf "icmp %s %s %s" style argsType argsStr
        | Call ->
            let functionName = args |> List.head |> (snd>>stringifyValue)
            let argsStr = args |> List.skip 1 |> List.map (fun (t,v) -> sprintf "%s %s" (stringifyType t) (stringifyValue v)) |> Strings.separatedBy ", "
            sprintf "call %s %s(%s)" returnTypeStr functionName argsStr
