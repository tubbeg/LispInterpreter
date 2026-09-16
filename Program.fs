open System
open Lexer
open System.IO


let parse tokens =
    tokens |> printfn "%A"

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