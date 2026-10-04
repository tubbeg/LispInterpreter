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


let getTokensUntilNextDeparan tokens =
    let rec gtund (tkens : Token list) enparanCounter (result : Token list)  =
        match tkens, enparanCounter with
        | [],_ -> result, []
        | Token.ENPARAN::rem,_ -> [Token.ENPARAN] |> List.append result |> gtund rem (enparanCounter + 1)
        | Token.DEPARAN::rem,0 -> result,rem
        | Token.DEPARAN::rem, x when x > 0 ->
            [Token.DEPARAN] |> List.append result |> gtund rem (enparanCounter - 1)
        | token::rem,_ -> [token] |> List.append result |> gtund rem enparanCounter
    gtund tokens 0 [] 




let addAtomToAst  ast   atom : Sexpression =
    [atom] |> List.append ast

let addNumberToAst  (ast : SexpItem list) nr =
    Number nr |> Atom |> addAtomToAst  ast

let addStringToAst  (ast : SexpItem list) s  =
    String s |> Atom |> addAtomToAst  ast

let addSymbolToAst  (ast : SexpItem list) sy =
    Symbol sy |> Atom |> addAtomToAst  ast 

let filterSpace tokens =
    tokens |> List.filter (fun e -> e<>Space)

let parse2 (tokens : seq<List<Token>>) : Sexpression =
    let rec ptkens (tkens : Token list, ast  : Sexpression) : (Token list * Sexpression) = 
        match tkens with
        | [] -> ([],ast)
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
            let newList = []
            let rem, subAst = ptkens (rem, newList)
            let appendList = subAst |> List.append ast
            rem, appendList
        | _ -> new Exception("problema") |> raise
    let tokenList = tokens |> seqListToList |> filterSpace
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