module TestGame

open QuestDriver

let player = { HP = 1.; Attack = 0.1; Defense = 0.2; Inventory = [] }
let crest = Crest("An old crest")
let key = Key("Tiny key")
let knight = Knight({HP = 0.5; Attack = 0.1; Defense = 0.}, "Dark and angry Knight")
let room1 = { id = 0; objects = [crest; key]; subjects = [knight]; description = "You are standing in a large hall with miracles on every wall" }
let room2 = { id = 1; objects = []; subjects = []; description = "Finish room"}
let world = { rooms = [|room1; room2|]; adjacency = [(0, 1)]; finishRoom = 1 }
let game = { currentPlayer = player; currentRoom = 0 }

describeState world game