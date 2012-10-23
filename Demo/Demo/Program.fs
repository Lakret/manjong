open System

let a = 300
let b = 500.5

let str = "This is BlackJack!"

let delimiter = ' '

let f = (fun x -> x * 2) //int -> int

let g x y = x * y
let g' = g 20

Array.iter (printfn "-- %c") <| str.ToCharArray()

[| 0..100 |] |> Array.min

let gen = new Random()

[| for _ in 0..100 -> gen.NextDouble() * 100.0 - 50.0 |]
|> Array.fold (fun acc elem -> if elem < acc then elem else acc) Double.PositiveInfinity

let rec fold f init arr =
    match arr with
    | [||] -> init
    | _ -> fold f (f init arr.[0])  arr.[1..]

fold (+) 0 [|1..100|]




//"hello world"
//" hello Dmitriy"
