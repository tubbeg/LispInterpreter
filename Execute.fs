module Execute
open System
open Parser
open System.Dynamic
//open FSharp.Interop.Dynamic



let addAtomNr acc next  =
    match acc, next with
    | Atom (Number nr1), Atom (Number nr2) -> nr1 + nr2 |> Number |> Atom
    | _ -> new Exception("") |> raise

let subAtomNr acc next   =
    match acc, next with
    | Atom (Number nr1), Atom (Number nr2) -> nr2 - nr1 |> Number |> Atom
    | _ -> new Exception("") |> raise

let execAdd (l : Sexpression) =
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

type FnExpression = {body:Sexpression;args:string;name:string}
type Property = {body:SexpItem;name:string}
type Expression =
    | Function of FnExpression
    | Property of Property
type SymbolMap = Map<string, Expression>

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


let createVariable (expression : Sexpression) sm =
    let rec iterExp (e : Sexpression) symMap (name : string option) (body : SexpItem option) : (Sexpression*SymbolMap) =
        match e, name, body with
        | [],Some _,Some _ -> [], symMap
        | Empty::rem,_,_ -> iterExp rem symMap name body
        | Atom (Symbol n)::rem,None,None ->
            //printfn "Found name: %A" n
            iterExp rem symMap (Some n) body
        | exp::rem,Some n,None ->
            //printfn "Found name: %A" n
            //printfn "Found body: %A" exp
            let bd = Property {body=exp;name=n}
            //printfn "Created property %A" bd
            let newMap = symMap |> Map.add n bd
            rem, newMap
        | c,_,_ ->
            printfn "Next is: %A" c
            new Exception("problem") |> raise
    iterExp expression sm None None


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


let isSexpression i =
    match i with
    | S _ -> true
    | _ -> false


let processIfS callback i sm result =
    match i with
    | S exp -> callback exp sm result
    | _ -> i




//anonymous record + lambda functions = dynamic object
let execute (ast : Sexpression) =
    ast |> printfn "Got AST: %A"
    //let symbolMap = ExpandoObject()
    //symbolMap?fns <- Map.empty
    //symbolMap?props <- Map.empty
    let rec processAst (sexp : Sexpression) (symMap : SymbolMap) (result : SexpItem) : SexpItem =
        let evalRem l =
            l |> List.map (fun i -> processIfS processAst i symMap result)
        match sexp with
        | Atom print::rem when isPrint print -> rem |> evalRem |> printFn
        | Atom (Symbol "+")::rem -> rem |> evalRem |> execAdd
        | Atom (Symbol "-")::rem -> rem |> evalRem |> execSub
        | S expression::rem -> Empty |> processAst expression symMap |> processAst rem symMap
        | Atom (Symbol "[d; e; f]")::rem ->
            let newRem,sm = createVariable rem symMap
            //printfn "Created variable %A" sm
            //printfn "Remaining: %A" newRem
            processAst newRem sm result
        | [] -> result
        | c::_ ->
            printfn "Encountered item %A" c
            new Exception("Problem") |> raise
    let result = processAst ast Map.empty Empty
    printfn "Done!"
    printfn "Result %A" result


