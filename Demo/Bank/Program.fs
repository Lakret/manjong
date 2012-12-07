open System
open System.Collections.Generic

let getAccountAmount =
    let rndGen = new Random()
    (fun() -> rndGen.NextDouble() * 1e9)

//let db = new Dictionary<string, string option * float>()
//db.Add("John", (Some "pass", getAccountAmount()))
//db.["John"]
//if db.ContainsKey "Helen" then db.["Helen"] else failwith "Have not Helen!111"

[<EntryPoint>]
let main _ = 
    let str = Console.ReadLine()
    printfn "Hello, %s, Echo + 10.5: %f" "Slow" <| getAccountAmount()
    0 // return an integer exit code
