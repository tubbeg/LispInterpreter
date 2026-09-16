module Lexer

open System
open System.IO

type Token =
    | Symbol of string
    | String of string
    | Number of int
    | Space
    | ENPARAN
    | DEPARAN

let startsWithNumber(w : string) =
    match String.length w with
    | x when x=0 -> false
    | _ -> w |> seq |> Seq.head |> Char.IsAsciiDigit

let startsWithLetter(w : string) =
    match String.length w with
    | x when x=0 -> false
    | _ -> w |> seq |> Seq.head |> Char.IsAsciiLetter

let isInvalidChar c =
    match c, Char.IsAsciiLetter c,Char.IsAsciiDigit c with
    | x,false, false when x = '_' -> false
    | _, true, _ -> false
    | _, _,true -> false
    | _ -> true

let isValidChar c = isInvalidChar c |> not

let symbolHasValidChars (w : string) =
    w |> seq |> Seq.forall (fun c -> isValidChar c)

let isSymbol w =
    match symbolHasValidChars w, startsWithLetter w with
    | true, true -> true
    | _ -> false

let isNumber w = w |> Seq.forall Char.IsDigit

let matchWord w =
    match w,isSymbol w, isNumber w with
    | "",_,_ -> Space
    | " ",_,_ -> Space
    | "(",_,_ -> ENPARAN
    | ")",_,_ -> DEPARAN
    | _,true,_ -> Symbol w
    | _,_,true -> w |> int |> Number
    | _ ->
        new Exception("Cannot parse" + w) |> raise

let lex (content : seq<string>) =
    seq {
        for c in content do
            let x = c.Split [|' '|]
            for y in x do
                yield y |> matchWord
    }