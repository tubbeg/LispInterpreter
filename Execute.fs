module Execute
open System
open Parser
open System.Dynamic
//open FSharp.Interop.Dynamic

type FnExpression = {body:Sexpression;args:string;name:string}
type VarValue =
    | Function of FnExpression
    | Property of SexpItem
type SymbolMap = Map<string, VarValue>

let throwUnexpectedSymbol sy =
    let errMsg = "Did not expected symbol: " + sy
    new Exception(errMsg) |> raise

let throwUnexpectedItem (i : SexpItem) =
    let errMsg = "Did not expected item: " + (i |> string)
    new Exception(errMsg) |> raise
let throwExpNumber () = new Exception("Expected number") |> raise
let throwWrongNrOfArgs () = new Exception("Wrong nr of arguments") |> raise

let atomToNr a =
    match a with
    | Atom (Number n) -> n
    | _ -> throwExpNumber ()

let subAtoms list =
    let l = List.rev list
    match l |> List.length with
    | 0 -> throwWrongNrOfArgs ()
    | 1 ->
        match List.head l with
        | Atom (Number n) -> 0 - n |> Number |> Atom
        | _ -> throwExpNumber ()
    | _ -> 
        let rec sa list result =
            match list with
            | [] -> result |> Number |> Atom
            | Atom (Number n)::rem ->
                printfn "subbing2 %A with %A" result n
                result - n |> sa rem
            | _ -> throwExpNumber ()
        sa (l |> List.tail) (l |> List.head |> atomToNr)

let addAtoms a1 a2 =
    match a1,a2 with
    | Atom (Number n), Atom (Number n2) -> n + n2 |> Number |> Atom
    | _ -> () |> throwExpNumber

type Operation =
    | Add
    | Sub

let functionCall (e : SexpItem) op =
    printfn "Executing %A %A" e op
    match op,e with
    | Add, S l  -> l |> List.fold addAtoms (Atom (Number 0))
    | Sub, S l  -> l |> subAtoms
    | Add, Atom (Number n) -> Atom (Number n)
    | Sub, Atom (Number n) -> Atom (Number (0 - n))
    | _ -> throwUnexpectedItem e

let execute (ast : Sexpression) =
    ast |> printfn "Got AST: %A"
    
    let rec eval (g : Sexpression) r : SexpItem =
        match g with
        | [] -> r
        | Empty::rem -> eval rem r
        | S e::rem -> r |> eval e |> eval rem
        | Atom (Number n)::rem ->
            let newR =
                match r with
                | S e -> e
                | Atom (Number n) -> Atom (Number n)::[]
                | _ -> []
            Atom (Number n)::newR |> S |> eval rem
        | Atom (Symbol sy)::rem -> 
            match sy with
            | "+" -> functionCall (eval rem r) Add
            | "-" -> functionCall (eval rem r) Sub
            | _ -> throwUnexpectedSymbol sy
        | c ->
            printfn "Got %A" c
            new Exception("Unexpected error") |> raise
            
    let result  = eval ast Empty

    printfn "\nDone!"
    printfn "Result %A" result


