module Lexer

open System
open System.IO
open System.Text.RegularExpressions

type Token =
    | Digit of char
    | Character of char
    | Space
    | Plus
    | Minus
    | Slash
    | Star
    | Enparan
    | Deparan





let parseCharacter c = 
    match c with
    | '(' -> Enparan
    | ')' -> Deparan
    | ' ' -> Space
    | '+' -> Plus
    | '-' -> Minus
    | '/' -> Slash
    | '*' -> Star
    | x when Char.IsAsciiDigit x -> x |> Digit
    | x when Char.IsLetter x -> x |> Character
    | _ ->
        printfn "Failed to parse %A" c
        new Exception() |> raise

let processString s =
    s |> Seq.toList |> List.map (fun c -> parseCharacter c)

let lex (content : seq<string>) : Token list =
    [for s in content do
        for token in processString s do
            yield token]
        