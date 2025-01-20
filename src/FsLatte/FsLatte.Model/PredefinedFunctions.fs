namespace FsLatte.Model
module public PredefinedFunctions =
    open Abs 
    let public functionsList: (string * Type) list =
            [
                ("printInt", Fun(Void,[Int]))//void printInt(int)
                ("printString", Fun(Void,[Str]))//void printString(string)
                ("error", Fun(Void,[]))//void error()
                ("readInt", Fun(Int,[]))//int readInt()
                ("readString", Fun(Str,[]))//string readString()
                ("concat", Fun(Str, [Str; Str]))//declare i8* @concat(i8*, i8*)
            ]