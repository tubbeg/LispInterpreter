open System
open System.IO

type Token =
    | Symbol of string
    | String of string
    | Number of int
    | Space

let startWithNumber (w : string) =
    let s = w |> seq
    for char in s do
        match Char.IsDigit char with 
        | true ->  ()
        | _ -> ()

let isSymbol w = false

let matchWord w =
    let ex = Exception("Cannot parse string")
    match w,isSymbol w with
    | "",_ -> Space
    | " ",_ -> Space
    | _, true -> Symbol w
    | _ -> ex |> raise

let lex (content : seq<string>) =
    seq {
        for c in content do
            c |> printfn "seq%A"
            yield c |> matchWord
    }
        
let parse tokens =
    ()

let execute ast =
    ()

let readSourceContent (path : string) = 
    let content = seq {
        use sr = new StreamReader (path)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    content |> printf "File contents: %A"
    content


let interpret() =
    let filePath = Environment.GetCommandLineArgs() |> Array.tail
    filePath
        |> Array.head
        |> readSourceContent
        |> lex
        |> parse
        |> execute


interpret()