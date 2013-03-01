#r "Samples.Hadoop.TypeProviders.dll"
open Samples.Hadoop
open System
open System.Collections.Generic
open System.Linq

let randGen = new Random()

let mu data = Array.sum data / float data.Length
//σ, стандартное отклонение
let sigma data = Math.Sqrt((mu <| Array.map (fun x -> x * x) data) - (mu data) ** 2.)
let P μ σ sample = (1. / (σ * sqrt(2. * Math.PI))) * exp(-0.5 * ((sample - μ) / σ) ** 2.)

type HadoopData = HiveTypeProvider<"tryfsharp",Port=10000,DefaultTimeout=20000>
let hive = HadoopData.GetDataContext()

let irisData = hive.iris

let transformClass = 
    dict [ "Iris-setosa", 0; "Iris-versicolor", 1; "Iris-virginica", 2 ]

let irises =
    [|
        for iris in irisData.OrderBy(fun x -> randGen.Next()) -> 
            [| iris.sepalLength; iris.sepalWidth; iris.petalLength; iris.petalWidth |], transformClass.[iris.``class``]
    |]
    
let trainData = irises.[..99]
let testData = irises.[100..]

let learn data =
    [|
        for c in 0..2 ->
            [|
                let slice = Array.filter (fun (_, ``class``) -> ``class`` = c) data
                for i in 0..3 do
                    let subslice = slice |> Array.map (fun ((x : _ []), _) -> x.[i])
                    let muI = mu subslice   
                    let sigmaI = sigma subslice
                    yield muI, sigmaI
            |]
    |]

let learnedParams = learn trainData
learnedParams.[1].[2] // 1 - вид, 2 - параметр: sl, sw, pl, pw

let pY =
    [| 
        for c in 0..2 do
            let count = trainData |> Array.filter (fun (_, ``class``) -> ``class`` = c) |> Array.length |> float
            yield count / 100.
    |]

let classify (datum : float []) =
    [|
        for y in 0..2 ->
            log(pY.[y]) + (Array.map2 (fun x (mu, sigma) -> log <| P mu sigma x) datum learnedParams.[y] |> Array.sum), y
    |]
    |> Array.maxBy fst

classify <| fst testData.[1]

let successCount = 
    testData |> Array.filter (fun datum -> ((snd << classify) <| fst datum) = snd datum) |> Array.length
    
float successCount / float testData.Length //0.9387
