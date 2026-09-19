open System
open Lexer
open System.IO

type Atom =
    | Number of int
    | Symbol of string
    | String of string

type Sexpression =
    | Atom
    | S of Sexpression

//abstract syntax tree
type AST =
    | Expression of Sexpression
    | NothingToDo

let seqListToList (s : seq<List<Token>>) =
    [
        for l in s |> Seq.toList do
            for t in l do
                yield t
    ]


    //| Symbol of string
    //| String of string
    //| Number of int
    //| Space
    //| ENPARAN
    //| DEPARAN


let appendToAST ast t = NothingToDo

let appendSexpressionToAST ast s = NothingToDo

let parseSexpression tokenList =
    let rec prse l s r =
        match l with
        | [] -> r,s
        | Token.DEPARAN::rem -> rem, s
        | token::rem ->
            match token with
            | Token.ENPARAN -> prse rem (appendSexpressionToAST )
            | anyOther -> 
    let result = []
    let sExpression : Sexpression option = None
    prse tokenList sExpression result

let parse (tokens : seq<List<Token>>) : AST =
    let rec ptkens (tkens : Token list) (ast : AST) = 
        match tkens with
        | [] -> ast
        | Token.Space::rem -> ptkens rem ast
        | Token.Number nr::rem -> Number nr |> appendToAST ast |> ptkens rem
        | Token.String s::rem -> String s |> appendToAST ast |> ptkens rem
        | Token.Symbol s::rem -> Symbol s |> appendToAST ast |> ptkens rem
        | Token.ENPARAN::rem ->
            match parseSexpression rem with
            | rem2,Some exp ->
                exp |> appendSexpressionToAST ast |> ptkens rem2
            | _ -> new Exception("Problema") |> raise
        | err::_ -> new Exception("Unable to parse token" + string err) |> raise
    let initDefaultValue = NothingToDo
    ptkens (tokens |> seqListToList) initDefaultValue

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
        |> parse
        |> execute


interpret()