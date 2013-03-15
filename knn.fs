open System

type Point = 
    { coords : float array
      clazz : string }  

let euclideanDistance x y =
    Array.map2 (fun x y -> (x-y)**2.) x y 
    |> Array.sum 
    |> Math.Sqrt 

euclideanDistance [| 1.; 1. |] [| 3.; 3. |]

let kNN k metric world test =
    let closest =
        Array.map (fun x -> x, metric test x.coords) world
        |> Array.sortBy snd
        |> Array.map fst
    closest.[..(k-1)]
    |> Seq.countBy (fun x -> x.clazz) 
    |> Seq.maxBy fst
