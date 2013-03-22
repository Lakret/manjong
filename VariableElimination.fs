open System
open System.Collections.Generic

type Queue<'a>() =
    let mutable internalList : 'a list = []

    member x.Enqueue(v) = 
        internalList <- internalList @ [v]
        x

    member x.Dequeue() =
        let x = List.head internalList
        internalList <- List.tail internalList
        x

    override x.ToString() = sprintf "%A" internalList

type Graph = Dictionary<string, string list>

let BFS (g : Graph) s f =
    let q = Queue()
    let marked = new HashSet<string>()


//[<EntryPoint>]
//let main argv = 
//    printfn "%A" argv
//    0 // return an integer exit code
