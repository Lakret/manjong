open System
open System.Collections.Generic


type State = 
    | Admin
    | User of string

type Comand =
    | Create of (string -> float)
    | Sub of (State -> float -> float)

let db = new Dictionary<string, string option * float>()

let getAccountAmount =
    let rndGen = new Random()
    (fun() -> rndGen.NextDouble() * 1e9)
    
let create name =  
    let amount = getAccountAmount()
    db.Add(name, (None, amount))
    amount

let sub state amount =
    match state with 
    | Admin -> failwith "Admin have no account"
    | User name -> 
        if db.ContainsKey name then            
            let newAmount = snd db.[name] - amount
            db.[name] <- (fst db.[name], newAmount)
            newAmount 
        else failwith <| sprintf "There's no user with name %s" name   

let describe state = 
    match state with
    | Admin -> [Create create , "New User"]
    | User _ -> [Sub sub , "Withdrawl the given amount of money from the user's account"]

let login (_state : State) =
    printfn "Enter username" 
    let name = Console.ReadLine()
    match fst db.[name] with
    | Some pasword ->
        let userPass = Console.ReadLine()
        if pasword = userPass then User name
        else failwith "Password doesn't match"
    | None ->
        printfn "Please, specify your password"
        let password = Console.ReadLine()
        db.[name] <- (Some password, snd db.[name])
        User name

[<EntryPoint>]
let main(_) =
    printfn "Available commands: %A" <| describe Admin
    0
