open System
open System.Collections.Generic

//TODO: 
// - обработка исключений
// - выход
// - прикрутить монго

type State = 
    | Admin
    | User of string

type Comand =
    | Create of (State -> string -> float)
    | Sub of (State -> float -> float)
    | Help of (State -> (Comand * string) list)

let db = new Dictionary<string, string option * float>()

let getAccountAmount =
    let rndGen = new Random()
    (fun() -> rndGen.NextDouble() * 1e9)
    
let create state name =  
    match state with 
    | Admin ->
        let amount = getAccountAmount()
        db.Add(name, (None, amount))
        printfn "User [%s] with amount %.2f has been created" name amount
        amount
    | User _ -> failwith "User cannot create another user"

let sub state amount =
    match state with 
    | Admin -> failwith "Admin have no account"
    | User name -> 
        if db.ContainsKey name then            
            let currentAmount = snd db.[name]
            printfn "Currently you have $%f and want to withdrawl $%f" currentAmount amount
            let newAmount = currentAmount - amount
            db.[name] <- (fst db.[name], newAmount)
            printfn "Succesful, new amount is $%F" newAmount
            newAmount 
        else failwith <| sprintf "There's no user with name %s" name   

let rec describe state = 
    let helpDescription = Help describe, "Describe Available commands"
    match state with
    | Admin -> [Create create , "New User"; helpDescription]
    | User _ -> [Sub sub , "Withdrawl the given amount of money from the user's account"; helpDescription]

let login () =
    let requestPassword correctPassword succesState =
        printfn "Enter your password"
        let userpassword = Console.ReadLine()
        if userpassword = correctPassword then succesState
        else failwith "Incorrect password"

    printfn "Enter username" 
    let name = Console.ReadLine()
    if name = "admin" then
        requestPassword "root" Admin
    else
        match fst db.[name] with
        | Some pasword ->
            requestPassword pasword (User name)
        | None ->
            printfn "Please, specify your password"
            let password = Console.ReadLine()
            db.[name] <- (Some password, snd db.[name])
            User name

let logout () = login ()

let splitWithConversion (input : string) commandName f =
    let splitted = input.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
    if (splitted.Length = 2) && (splitted.[0] = commandName) then  
        let (success, value) = f splitted.[1]
        if success then Some(splitted.[0], value) else None
    else None

let (| StringWithAmount |_|) (input : string) = splitWithConversion input "sub" (Double.TryParse) 
let (| StringWithName |_|) (input : string) = splitWithConversion input "create" (fun x -> true, x) 

let executor state (input : string) =
    match input with
    | "help" -> 
        printfn "Available commands:"
        describe state |> List.iter (printfn "%A")
        state
    | "logout" -> logout ()
    | StringWithAmount(command, amount) -> 
        sub state amount |> ignore
        state
    | StringWithName(command, name) -> 
        create state name |> ignore  
        state
    | _ -> failwith "Unknown command"

let rec loopWithState f startState = loopWithState f <| f startState 

[<EntryPoint>]
let main(_) =
    let startState = login () 
    printfn "Current state %A" startState
    loopWithState (fun state -> executor state (Console.ReadLine())) startState
    0
