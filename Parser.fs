module Parser
open System
open Lexer
open System.Text.RegularExpressions


type Sexpression = SexpItem list
and SexpItem =
    | Atom of Atom
    | S of Sexpression
    | Empty
and Atom =
    | Number of int
    | Symbol of string
    | String of string

let seqCharToString (sequence : char seq) =
    sequence |> Seq.toList |> List.toArray |> System.String

let tokenNrToChar t =
    match t with 
    | Digit n -> n
    | c ->
        printfn "Got token %A" c
        new Exception("Did not expect non-number") |> raise


let tokenCharToChar t =
    match t with 
    | Token.Character c -> c
    | Token.Digit n -> n
    | c ->
        printfn "Got token %A" c
        new Exception("Did not expect non-number") |> raise

let tokenNumberListToNumber l : Atom =
    let i = l |> List.map (fun i -> tokenNrToChar i)
    let s = seq {for c in i do yield c}
    s |> seqCharToString |> int |> Number

let parseDigits l =
    let rec pad list result =
        match list with
        | Digit x::rem ->
            let newResult = [Digit x] |> List.append result
            pad rem newResult
        | rem -> result,rem
    let r,remainder = pad l []
    tokenNumberListToNumber r, remainder
let tokenCharListToSymbol l =
    printfn "Got list %A" l
    let head = l |> List.head
    match head with
    | Plus -> Symbol "+"
    | Minus -> Symbol "-"
    | Slash -> Symbol "/"
    | Star -> Symbol "*"
    | _ ->
        l |> List.map (fun i -> tokenCharToChar i) |> string |> Symbol

let isSpecChar sy =
    match sy with
    | Plus -> true
    | Minus -> true
    | Star -> true
    | Slash -> true
    | _ -> false

let isChar c =
    match c with
    | Character _ -> true
    | _ -> false

let firstTokenIsCharacter l =
    match l |> List.isEmpty with
    | true -> false
    | _ -> l |> List.head |> isChar

let firstTokenIsSpecChar l =
    match l |> List.isEmpty with
    | true -> false
    | _ -> l |> List.head |> isSpecChar


let parseSymbol l =
    let rec pad list result =
        match list with
        | Plus::rem -> 
            let newResult = [Plus] |> List.append result
            pad rem newResult
        | Minus::rem -> 
            let newResult = [Minus] |> List.append result
            pad rem newResult
        | Star::rem -> 
            let newResult = [Star] |> List.append result
            pad rem newResult
        | Slash::rem -> 
            let newResult = [Slash] |> List.append result
            pad rem newResult
        | Character c::rem ->
            let newResult = [Character c] |> List.append result
            pad rem newResult
        | Digit n::rem ->
            let newResult = [Digit n] |> List.append result
            pad rem newResult
        | rem -> result,rem
    match firstTokenIsCharacter l || firstTokenIsSpecChar l with 
    | true -> 
        let r,remainder = pad l []
        tokenCharListToSymbol r, remainder
    | _ ->
        l |> List.iter (fun c -> printfn "Got token: %A" c)
        new Exception("Invalid symbol") |> raise

    

//for now, there's no support for user-defined strings at all
let parse (tokens : List<Token>) : Sexpression =
    let rec ptkens (tkens : Token list, ast  : Sexpression) : (Token list * Sexpression) = 
        match tkens with
        | [] -> [],ast
        | Deparan::rem -> rem,ast
        | Digit x::rem ->
            let number,newRemainder = parseDigits (Digit x::rem)
            let updatedAst = [number |> Atom] |> List.append ast
            ptkens (newRemainder,updatedAst)
        | Character c::rem ->
            let sy,newRemainder = parseSymbol (Character c::rem)
            let updatedAst = [sy |> Atom] |> List.append ast
            ptkens (newRemainder,updatedAst)
        | Plus::rem ->
            let sy,newRemainder = parseSymbol (Plus::rem)
            let updatedAst = [sy |> Atom] |> List.append ast
            ptkens (newRemainder,updatedAst)
        | Star::rem ->
            let sy,newRemainder = parseSymbol (Star::rem)
            let updatedAst = [sy |> Atom] |> List.append ast
            ptkens (newRemainder,updatedAst)
        | Slash::rem ->
            let sy,newRemainder = parseSymbol (Slash::rem)
            let updatedAst = [sy |> Atom] |> List.append ast
            ptkens (newRemainder,updatedAst)
        | Minus::rem ->
            let sy,newRemainder = parseSymbol (Minus::rem)
            let updatedAst = [sy |> Atom] |> List.append ast
            ptkens (newRemainder,updatedAst)
        | Enparan::rem -> 
            let rem, subAst = ptkens (rem, [])
            let sExpression = subAst |> S
            let appendList3 = [sExpression] |> List.append ast
            ptkens (rem, appendList3)
        | Space::rem -> ptkens (rem,ast)
    let remainingTokens, result = ptkens (tokens,[])
    remainingTokens |> printfn "Remaining tokens %A"
    result