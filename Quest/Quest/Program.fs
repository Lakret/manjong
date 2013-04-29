module QuestDriver

open System
open System.Collections.Generic

type RoomObject = 
    | Crest of string //descriptions
    | Key of string

type NPC =
    { HP : float
      Attack : float
      Defense : float }

type Subject = 
    | Knight of NPC * string // string - description

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

type World = 
    { rooms : Room array
      adjacency : (int * int) list 
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
    | Crest(desc) -> desc
    | Key(desc) -> desc

let describeSubject = 
    function    
    | Knight(_,desc) -> desc

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
        
let makeAction command : Action = 
    failwith "Not Implemented"

let performAction world game action : World * Game = 
    failwith "Not Implemented"

//play : World * Game  -> World * Game * Result option 
let rec play world game =
    match checkResult world game with
    | Some x as res -> world, game, res
    | None -> 
        describeState world game
        let command = Console.ReadLine()
        let world, game = command |> makeAction |> performAction world game
        play world game



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
