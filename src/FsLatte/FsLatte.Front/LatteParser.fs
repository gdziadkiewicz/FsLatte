namespace FsLatte.Front
open FsLatte.Model.Abs
open Chessie.ErrorHandling;

type public ParsingResult = Result<Program, string>

module public LatteParser =
    open System.IO
    open Microsoft.FSharp.Text.Lexing
    open Par
    open Lex

    let parseFromTextReader (textReader:TextReader) : ParsingResult =
        try
            textReader
            |> LexBuffer<char>.FromTextReader
            |> pProgram token
            |> ok
        with BnfcUtil.ParseErrorException (start_pos, end_pos) ->
            sprintf "Parse error at %d.%d-%d.%d"
                start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
                end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
            |> fail

    let parseFromString (str:string) : ParsingResult =
        try
            str 
            |> LexBuffer<char>.FromString
            |> pProgram token
            |> ok
        with BnfcUtil.ParseErrorException (start_pos, end_pos) ->
            sprintf "Parse error at %d.%d-%d.%d"
                start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
                end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
            |> fail

