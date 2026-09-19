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

let isDigit dop =
    match dop with
    | Some c -> c |> Char.IsAsciiDigit
    | _ -> false

let isLetter lop  =
    match lop with
    | Some c -> c |> Char.IsAsciiLetter
    | _ -> false


let startsWithNumber(w : string) =
    match String.length w with
    | x when x=0 -> false
    | _ -> w |> seq |> Seq.tryHead |> isDigit
let charIsSpecialCharacter c =
    match c with
    | '+' -> true
    | '-' -> true
    | '*' -> true
    | '/' -> true
    | '_' -> true
    | _ -> false

let startsWithLetterOrSpecialChar(w : string) =
    match String.length w with
    | x when x=0 -> false
    | _ ->
        match charIsSpecialCharacter w[0] with
        | true -> true
        | _ -> w |> seq |> Seq.tryHead |> isLetter

//caress

let isInvalidChar c =
    (Char.IsAsciiLetter c || Char.IsAsciiDigit c || charIsSpecialCharacter c) |> not

let isValidChar c = isInvalidChar c |> not

let symbolHasValidChars (w : string) =
    w |> seq |> Seq.forall (fun c -> isValidChar c)

let isSymbol w =
    symbolHasValidChars w && startsWithLetterOrSpecialChar w

let isNumber w = w |> Seq.forall Char.IsDigit

let startsWithParan (w : string) =
    let result = w |> seq |> Seq.tryHead 
    match result with 
    | Some r -> r ='('
    | _ -> false

let endWithsParan (w : string) =
    match w |> seq |> Seq.tryLast with
    | Some r -> r = ')'
    | _ -> false

let symToList s  = [s]

let splitExpression e : string list =
    match startsWithParan e, endWithsParan e with
    | true,_ ->
        let result = e.[1..]
        ["("; result]
    | _,true ->
        match e.Length with
        | x when x > 1 ->
            let s = e.Remove(e.Length - 1)
            [s; ")"]
        | _ -> new Exception("Invalid length" + e) |> raise
    | _ -> new Exception("Invalid string" + e) |> raise

let hasParan w =
    startsWithParan w || endWithsParan w

let matchWord (word : string) =
    let rec ms que r =
        match que with
        | [] -> r
        | s::rem ->
            match s,isSymbol s, isNumber s, hasParan s with
            | "",_,_,_ -> [Space] |> List.append r |> ms rem
            | " ",_,_,_ -> [Space] |> List.append r |> ms rem
            | "(",_,_,_ -> [ENPARAN] |> List.append r |> ms rem
            | ")",_,_,_ -> [DEPARAN] |> List.append r |> ms rem
            | _,true,_,_ -> [Symbol s] |> List.append r |> ms rem
            | _,_,true,_ -> [s |> int |> Number] |> List.append r |> ms rem
            | _,_,_,true ->
                let newExpression = splitExpression s
                let appendRem = newExpression |> List.append rem
                ms appendRem r
            | _ ->
                let resultOfSymbol = isInvalidChar s[0]
                new Exception("Cannot parse: " + s + string(resultOfSymbol)) |> raise
    let result = ms [word] []
    result

let lex (content : seq<string>) =
    seq {
        for c in content do
            let x = c.Split [|' '|]
            for y in x do
                yield y |> matchWord
    }