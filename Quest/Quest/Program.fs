module QuestDriver

open System
open System.Collections.Generic

type RoomObject = 
    | Chest of string //descriptions
    | Key of string

type NPC =
    { HP : float
      Attack : float
      Defense : float }

type Subject = 
    //TODO: в домашке вернуть NPC
    //| Knight of NPC * string // string - description
    | Knight of string

type Player = 
    { HP : float 
      Attack : float
      Defense : float
      Inventory : RoomObject list }

type Room = 
    { id : int
      objects : RoomObject list
      subjects : Subject list
      description : string }

type Direction = 
    | Up | Down | Left | Right

    member x.Invert = 
        match x with 
        | Up -> Down
        | Down -> Up
        | Left -> Right
        | Right -> Left

type World = 
    { rooms : Room array
      adjacency : (int * int * Direction) list 
      finishRoom : int
    }

type Game =
    { currentRoom : int
      currentPlayer : Player }

type Action = 
    | RoomAction of (Room -> Player -> Room * Player)
    | MoveAction  of (Game -> Player -> Game * Player)

type Result = | Lost | Win

let checkResult world game =
    if game.currentPlayer.HP = 0. then Some Lost
    elif game.currentRoom = world.finishRoom then Some Win
    else None

let describePlayer player = 
    printfn "Your current HP is %i" << int <| player.HP * 100.

let describeObject =
    function
    | Chest(desc) -> desc
    | Key(desc) -> desc

let describeSubject = 
    function    
    //| Knight(_,desc) -> desc
    | Knight x -> x

//player description
let describeState world game =
    let room = world.rooms.[game.currentRoom]
    //room description
    printfn "%s" <| room.description
    //objects description
    printfn "In the room you see:"
    for currObj in room.objects do 
        printfn "\t — %s" <| describeObject currObj
    //subjects description
    for subj in room.subjects do 
        printfn "\t — %s" <| describeSubject subj
    describePlayer game.currentPlayer

type MoveActions = 
    | Go of Direction

type RoomActions = 
    | Open
    | Take
    | Attack

type ActionDescriptor = 
    | MoveActionDesc of MoveActions
    | RoomActionDesc of RoomActions

let (|ActionWithParams|_|) command (str : string) = 
    match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
    | x::xs when x = command -> Some xs
    | _ -> None
       
let makeAction = 
    function
    | ActionWithParams "go" [ direction ] -> 
        let directionDesc = 
            match direction with
            | "up" -> Up
            | "down" -> Down
            | "left" -> Left
            | "right" -> Right
            | _ -> failwith "Incorrect direction"
        MoveActionDesc(Go(directionDesc))
    | "open" -> RoomActionDesc(Open)
    | "take" -> RoomActionDesc(Take)
    | "attack" -> RoomActionDesc(Attack)
    | _ -> failwith "Incorrect command"

let buildActionFromActionDesc world =
    function 
    | MoveActionDesc(Go(dir)) ->
        MoveAction(fun game player -> 
            try 
                let newRoom = 
                    world.adjacency
                    |> List.filter (fun (x, _, adjDir) -> (x = game.currentRoom) && adjDir = dir)
                    |> List.map (fun (_, y, _) -> y)
                    |> List.head
                { game with currentRoom = newRoom }, player
            with 
                | _ -> failwith "There are no passage in this direction")
    // TODO: домашка, сделать так, чтобы в сундуке что-то было и Open работало только в комнате, где есть сундук,
    // и если у игрока есть ключ. если всё хорошо, добавляем в currentPlayer.Inventory объект из сундука
    | RoomActionDesc(Open) -> RoomAction(fun room player -> room, player)
    | RoomActionDesc(Take) -> 
        RoomAction(fun room player ->
            let isKey = function | Key(_) -> true | _ -> false
            if room.objects |> List.exists isKey then
                { room with objects = room.objects |> List.filter (isKey >> not) }, { player with Inventory = [ Key("Key for crest") ] }
            else room, player)
    // TODO: домашка
    | RoomActionDesc(Attack) -> failwith "Not implemented yet"

let performAction actionDesc game world =
    match actionDesc |> buildActionFromActionDesc world with
    | MoveAction(f) ->
        let game, _ = f game game.currentPlayer
        game, world
    | RoomAction(f) ->
        let room, player = f world.rooms.[game.currentRoom] game.currentPlayer
        world.rooms.[game.currentRoom] <- room
        { game with currentPlayer = player }, world

let safeRetry f game world =
    try f game world with
    | e -> 
        printfn "Wrong! %s" e.Message
        game, world


//play : World * Game  -> World * Game * Result option 
let rec play world game =
    match checkResult world game with
    | Some _ as res -> world, game, res
    | None -> 
        describeState world game
        let command = Console.ReadLine()
        let game, world = safeRetry (command |> makeAction |> performAction) game world
        play world game

let testWorld = 
    let defRoom id = {id = id; objects = []; subjects = []; description = (id |> string) }
    {
        rooms = 
            [| 
                for i in 0..4 ->
                    match i with
                    | 1 -> { defRoom i with subjects = [ Knight("Angry knight") ] }
                    | 2 -> { defRoom i with objects = [ Chest("Black chest") ] }
                    | 3 -> { defRoom i with objects = [ Key("Golden Key") ] }
                    | _ -> defRoom i
            |]
        adjacency = 
            [
                let dirs = [ Up; Right; Down; Left ] |> List.mapi (fun idx x -> idx + 1, x)
                for (idx, dir) in dirs do
                    yield! [ (0, idx, dir); (idx, 0, dir.Invert) ]
            ]
        finishRoom = 4
    }   
    
let testGame = 
    {
        currentRoom = 0
        currentPlayer = { HP = 1.; Attack = 0.2; Defense = 0.1; Inventory = [] }
    } 

[<EntryPoint>]
let main argv = 
    play testWorld testGame |> ignore
    printfn "You won! Congratulations!"
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
