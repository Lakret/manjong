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
    |> Seq.maxBy snd

let world =
    [|
        {coords = [|1.; 3.|]; clazz = "red"}
        {coords = [|2.; 3.|]; clazz = "red"}
        {coords = [|2.; 2.|]; clazz = "red"}
        {coords = [|2.; 1.|]; clazz = "red"}
        {coords = [|4.; 1.|]; clazz = "red"}
        {coords = [|0.; 0.|]; clazz = "green"}
        {coords = [|0.; 1.|]; clazz = "green"}
        {coords = [|1.; 1.|]; clazz = "green"}
        {coords = [|1.; 0.|]; clazz = "green"}
        {coords = [|2.; 0.|]; clazz = "green"}
        {coords = [|3.; 4.|]; clazz = "green"}
        {coords = [|4.; 4.|]; clazz = "green"}
        {coords = [|4.; 3.|]; clazz = "green"}
    |]

let test = [|3.; 1.|]

//cross validation
let CV min max n world =
    //TODO: выбирает оптимальное минимальное значение k, при котором для случайного разбиения мира на (world.Length - n) известный точек и 
    //n вопросиков, больше всего вопросиков получают правильный класс после применения kNN
