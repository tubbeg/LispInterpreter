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

let addAtomNr acc next  =
    match acc, next with
    | Atom (Number nr1), Atom (Number nr2) -> nr1 + nr2 |> Number |> Atom
    | item ->
        printfn "Got %A" item
        new Exception("Expected numbers") |> raise

let subAtomNr acc next   =
    match acc, next with
    | Atom (Number nr1), Atom (Number nr2) -> nr2 - nr1 |> Number |> Atom
    | _ -> new Exception("") |> raise

let execAdd (l : Sexpression) =
    printfn "Adding %A" l
    let initState = Number 0 |> Atom
    l |> List.fold (fun s t -> addAtomNr t s) initState

let execSub (l : Sexpression)  =
    match l with
    | [] -> Empty
    | Atom (Number n)::rem -> 
        let initState = Number n |> Atom
        rem |> List.fold (fun s t -> subAtomNr t s) initState
    | _ -> new Exception("Not a number") |> raise

let parseName l =
    match l with
    | Atom (Symbol name)::rem -> name,rem
    | _ -> new Exception("Failed to parse name") |> raise

let parseArg l =
    match l with
    | Atom (Symbol arg)::rem -> arg,rem
    | _ -> new Exception("Failed to parse name") |> raise

let parseFnBody l =
    match l with
    | S body::rem -> body,rem
    | _ -> new Exception("Failed to parse name") |> raise

(*
let createLet (l : Sexpression) (symMap : SymbolMap) : (SymbolMap*Sexpression) =
    let name,rem = parseName l
    let args,newRem = parseArg rem
    let fnBody,remainder = parseFnBody newRem
    let newFn = {body=fnBody;args=args;name=name}
    printfn "Created function with %A" newFn
    let newMap = symMap |> Map.add name newFn
    newMap,remainder
*)
let isFunctionCall symbol (symMap : SymbolMap) = true


type EvalFn = Sexpression -> SymbolMap -> Sexpression  -> Sexpression

let callFunction symbol (symMap : SymbolMap) (evalFn : EvalFn) : SexpItem =
    let fn = symMap |> Map.tryFind symbol
    match fn with
    | Some func ->
        //let body = func.body
        //let args = Symbol func.args
        //let result = evalFn body symMap []
        Empty
    | None -> new Exception("problem calling function") |> raise



//it should be possible to use expandoobjects instead of storing the actual atom
(*
let createVariable (state : InterpreterState) =
    let rec iterExp (s : InterpreterState) (name : string option) (body : SexpItem option) : InterpreterState =
        match s.list, name, body with
        | [],Some _,Some _ -> s
        | Empty::rem,_,_ -> iterExp {list=rem; result=s.result;sym=s.sym} name body
        | Atom (Symbol n)::rem,None,None ->
            //printfn "Found name: %A" n
            iterExp {list=rem; sym=s.sym;result=s.result} (Some n) body
        | item::rem,Some nm,None ->
            printfn "Found name: %A" nm
            printfn "Found body: %A" item
            let bd = Property item
            printfn "Created property %A with key %A" bd nm
            let newMap = s.sym |> Map.add nm bd
            printfn "New map created with: %A" newMap
            printfn "Remaining atoms: %A" rem
            {list=rem; sym=newMap; result=s.result}
        | c,_,_ ->
            printfn "Next is: %A" c
            new Exception("problem") |> raise
    iterExp state None None

*)

let printFn l =
    let rec pfn expression =
        match expression with
        | [] -> Empty
        | c::rem -> 
            printfn ": %A" c
            pfn rem
    pfn l


let isPrint symbol =
    //printfn "Checking %A" symbol
    match symbol with
    //this is really bad
    //when converting from tokens to symbols, the
    //actual strings are converted from lists to this ...
    //but on the bright side, the print function actually works!
    | Symbol "[p; r; i; ... ]" -> true     
    | _ -> false

let isVariable (i : SexpItem) (sm : SymbolMap)  =
    match i with
    | Atom (Symbol name) ->
        let result = sm |> Map.tryFind name
        printfn "Got var search result: %A" result
        printfn "Searched for: %A" i
        printfn "Map %A" sm
        result
    | _ -> None

let processIfv item sm =
    match isVariable item sm with
    | Some (Property value) ->
        printfn "Found variable with value %A" value
        value
    | _ ->
        printfn "Found item with value %A" item
        match item with
        | Atom (Symbol "[v]") -> printfn "Found it! %A" item
        | _ -> ()
        item




//procedure DFS(G, v) is
//    label v as discovered
//    for all directed edges from v to w that are in G.adjacentEdges(v) do
//        if vertex w is not labeled as discovered then
//            recursively call DFS(G, w)

(*
def dfsRec(adj, visited, s, res):
    visited[s] = True
    res.append(s)

    # Recursively visit all adjacent vertices 
    # that are not visited yet
    for i in adj[s]:
        if not visited[i]:
            dfsRec(adj, visited, i, res)


def dfs(adj):
    visited = [False] * len(adj)
    res = []
    dfsRec(adj, visited, 0, res)
    return res*)

(*
procedure DFS_iterative(G, v) is
    let S be a stack
    S.push(v)
    while S is not empty do
        v = S.pop()
        if v is not labeled as discovered then
            label v as discovered
            for all edges from v to w in G.adjacentEdges(v) do 
                S.push(w)*)

//anonymous record + lambda functions = dynamic object //except anon records has limitations





let throwExpNumber () = new Exception("Expected number") |> raise


let execute (ast : Sexpression) =
    ast |> printfn "Got AST: %A"
    
    //WHY IS THIS NOT WORKING?

    //Because you need to traverse a tree

    //Evaluate the leaves first, and then move upwards!! To the root

    //https://www.geeksforgeeks.org/dsa/level-order-tree-traversal/

    //ANVÄND DFS

    //DU MÅSTE HA ETT ID FÖR VARJE S EXPRESSION

    let rec eval (g : Sexpression) r : SexpItem =

        let add first second =
            match first,second with
            | Atom (Number n), Atom (Number n2) -> n + n2 |> Number |> Atom
            | Atom (Number n),S ex ->
                match eval ex r with
                | Atom (Number n2) -> n + n2 |> Number |> Atom
                | _ -> () |> throwExpNumber
            | S ex,Atom(Number n) ->
                match eval ex r with
                | Atom (Number n2) -> n + n2 |> Number |> Atom
                | _ -> () |> throwExpNumber
            | _ -> () |> throwExpNumber
        match g with
        | [] -> r
        | Empty::rem -> eval rem r
        | Atom (Number n)::_ -> Atom (Number n)
        | Atom (Symbol "+")::rem -> rem |> List.reduce add
        | S e::rem -> eval e r |> eval rem
        | c ->
            printfn "Got %A" c
            new Exception("Unexpected error") |> raise
            
    let result  = eval ast Empty

    printfn "\nDone!"
    printfn "Result %A" result


