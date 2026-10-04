open System
open Lexer
open System.IO

type Sexpression = SexpItem list
and SexpItem =
    | Atom of Atom
    | S of Sexpression
    | Empty
and Atom =
    | Number of int
    | Symbol of string
    | String of string

let seqListToList (s : seq<List<Token>>) =
    [
        for l in s |> Seq.toList do
            for t in l do
                yield t
    ]

let filterSpace tokens =
    tokens |> List.filter (fun e -> e<>Space)

let parse2 (tokens : seq<List<Token>>) : Sexpression =
    let rec ptkens (tkens : Token list, ast  : Sexpression) : (Token list * Sexpression) = 
        match tkens with
        | [] -> [],ast
        | Token.DEPARAN::rem -> rem,ast
        | Token.Number nr::rem ->
            let updatedAst = [nr |> Number |> Atom] |> List.append ast
            ptkens (rem,updatedAst)
        | Token.String s::rem -> 
            let updatedAst = [s |> String |> Atom] |> List.append ast
            ptkens (rem,updatedAst)
        | Token.Symbol sy::rem ->
            let updatedAst = [sy |> Symbol |> Atom] |> List.append ast
            ptkens (rem,updatedAst)
        | Token.ENPARAN::rem -> 
            let rem, subAst = ptkens (rem, [])
            let sExpression = subAst |> S
            let appendList3 = [sExpression] |> List.append ast
            ptkens (rem, appendList3)
        | _ -> new Exception("problema") |> raise
    let tokenList = tokens |> seqListToList |> filterSpace //|> addExtraS
    printfn "New tokens list %A" tokenList
    let remainingTokens, result = ptkens (tokenList,[])
    remainingTokens |> printfn "Remaining tokens %A"
    result

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
        |> parse2
        |> execute


interpret()