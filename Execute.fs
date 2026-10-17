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


//anonymous record + lambda functions = dynamic object //except anon records has limitations

let throwExpNumber () = new Exception("Expected number") |> raise

let addAtoms a1 a2 =
    match a1,a2 with
    | Atom (Number n), Atom (Number n2) -> n + n2 |> Number |> Atom
    | _ -> () |> throwExpNumber

let subAtoms a1 a2 =
    match a1,a2 with
    | Atom (Number n), Atom (Number n2) ->
        printfn "subbing %A with %A" n n2
        n - n2 |> Number |> Atom
    | _ -> () |> throwExpNumber

let isNumberAtom a =
    match a with
    | Atom (Number _) -> true
    | _ -> false

type EvalFn = Sexpression -> SexpItem -> SexpItem

let addSexpWithNumber (s : Sexpression) a r (cb : EvalFn) =
    match cb s r, a with
    | Atom (Number n2), Atom (Number n) -> n + n2 |> Number |> Atom
    | _ -> () |> throwExpNumber

let addSexpWithSexp (s1 : Sexpression) s2 r (cb : EvalFn) =
    match cb s1 r, cb s2 r with
    | Atom (Number n2), Atom (Number n) -> n + n2 |> Number |> Atom
    | _ -> () |> throwExpNumber

let subSexpWithSexp (s1 : Sexpression) s2 r (cb : EvalFn) =
    match cb s1 r, cb s2 r with
    | Atom (Number n2), Atom (Number n) -> n - n2 |> Number |> Atom
    | _ -> () |> throwExpNumber

let subSexpWithNumber (s : Sexpression) a r (cb : EvalFn) =
    match cb s r, a with
    | Atom (Number n2), Atom (Number n) -> n - n2 |> Number |> Atom
    | _ ->
        printfn "Got %A %A" s a
        () |> throwExpNumber

let add first second r (cb : EvalFn) =
    match first,second with
    | a1, a2 when isNumberAtom a1 && isNumberAtom a2 -> addAtoms a1 a2
    | Atom (Number n),S ex -> addSexpWithNumber ex (Atom (Number n)) r cb
    | S ex, Atom (Number n) -> addSexpWithNumber ex (Atom (Number n)) r cb
    | S ex1, S ex2 -> addSexpWithSexp ex1 ex2 r cb
    | _ -> () |> throwExpNumber

let sub first second r (cb : EvalFn) =
    match first,second with
    | a1, a2 when isNumberAtom a1 && isNumberAtom a2 -> subAtoms a1 a2
    | Atom (Number n),S ex -> subSexpWithNumber ex (Atom (Number n)) r cb
    | S ex, Atom (Number n) -> subSexpWithNumber ex (Atom (Number n)) r cb
    | S ex1, S ex2 -> addSexpWithSexp ex1 ex2 r cb
    | _ -> () |> throwExpNumber

let throwUnexpectedSymbol sy =
    let errMsg = "Did not expected symbol: " + sy
    new Exception(errMsg) |> raise

let throwUnexpectedItem (i : SexpItem) =
    let errMsg = "Did not expected item: " + (i |> string)
    new Exception(errMsg) |> raise

type Operation =
    | Add
    | Sub


let functionCall (e : SexpItem) op =
    printfn "Executing %A %A" e op
    match op,e with
    | Add, S l  -> l |> List.reduce addAtoms
    | Add, Atom (Number n) -> (Atom (Number n))
    | Sub, _ -> throwUnexpectedSymbol "Sub"
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
            let rr = Atom (Number n)::newR
            printfn "Appending %A" rr
            eval rem (S rr)
        | Atom (Symbol sy)::rem -> 
            let eval2 = eval rem r
            printfn "Here be: %A" eval2
            match sy with
            | "+" -> functionCall eval2 Add
            | _ -> throwUnexpectedSymbol sy
        | c ->
            printfn "Got %A" c
            new Exception("Unexpected error") |> raise
            
    let result  = eval ast Empty

    printfn "\nDone!"
    printfn "Result %A" result


