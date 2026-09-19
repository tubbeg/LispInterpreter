open System
open Lexer
open System.IO

type Stuff = Stuff

type AST =
    | S of Stuff
    | NothingToDo


let seqListToList (s : seq<List<Token>>) =
    [
        for l in s |> Seq.toList do
            for t in l do
                yield t
    ]

let parseTokens (tokens : seq<List<Token>>) =
    tokens |> printfn "Do tokens processing stuff here: %A"
    let tokenList = tokens |> seqListToList
    let rec ptkens (tkens : Token list) ast = 
        match tkens with
        | [] -> NothingToDo
        | token::rem ->
            printfn "Tokens is: %A" token
            ptkens rem NothingToDo 
    ptkens tokenList NothingToDo

let execute ast =
    ast |> printfn "Got AST: %A"
    printfn "Done!"

let readSourceContent (path : string) = 
    let content = seq {
        use sr = new StreamReader (path)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    content |> printfn "File contents: %A"
    content


let interpret() =
    let filePath = Environment.GetCommandLineArgs() |> Array.tail
    filePath
        |> Array.head
        |> readSourceContent
        |> lex
        |> parseTokens
        |> execute


interpret()