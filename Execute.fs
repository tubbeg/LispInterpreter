module Execute
open System
open Parser

let addAtomNr acc next  =
    match acc, next with
    | Atom (Number nr1), Atom (Number nr2) -> nr1 + nr2 |> Number |> Atom
    | _ -> new Exception("") |> raise

let subAtomNr acc next  =
    match acc, next with
    | Atom (Number nr1), Atom (Number nr2) -> nr2 - nr1 |> Number |> Atom
    | _ -> new Exception("") |> raise

let execAdd (l : Sexpression) =
    let initState = Atom (Number 0)
    l |> List.fold (fun s t -> addAtomNr t s) initState

let execSub (l : Sexpression) =
    let initState = Atom (Number 0)
    l |> List.fold (fun s t -> subAtomNr t s) initState

let createLet (l : Sexpression) =
    let initState = Atom (Number 0)
    l |> List.fold (fun s t -> subAtomNr t s) initState

let execute (ast : Sexpression) =
    ast |> printfn "Got AST: %A"
    let rec processAst (sexp : Sexpression) result : Sexpression =
        match sexp with
        | Atom (Symbol "+")::rem ->
            execAdd rem::[] |> List.append result
        | Atom (Symbol "-")::rem ->
            execSub rem::[] |> List.append result
        | Atom (Symbol "let")::rem ->
            createLet rem::[] |> List.append result
        | S expression::rem ->
            let res = processAst expression [] |> S
            let newResult = [res] |> List.append result
            processAst rem newResult
        | [] -> []
    let result = processAst ast []
    printfn "Done!"
    printfn "Result %A" result
