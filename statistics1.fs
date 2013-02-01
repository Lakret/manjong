open System
open System.Collections.Generic
open Samples.Charting.DojoChart

//рост
let data = [| 185.; 196.; 165.; 180.; 173.; 120.; 210. |]

let data2 = [| 650.; 1.; 668.; 804.; 650. |]
let data3 = [| 180.; 160.; 192.; 165.5; 200. |]



//μ, мат. ожидание
let μ data = Array.sum data / float data.Length
//σ, стандартное отклонение
let σ data = Math.Sqrt((μ <| Array.map (fun x -> x * x) data) - (μ data) ** 2.)
let P μ σ sample = (1. / (σ * sqrt(2. * Math.PI))) * exp(-0.5 * ((sample - μ) / σ) ** 2.)

let muHeight = μ data
let stdDevHeight = σ data 
let numOfStd = Array.filter (fun x -> abs(x - muHeight) <= stdDevHeight) data |> Seq.length
(float numOfStd) / ((float data.Length) / 100.)
let PHeight = P muHeight stdDevHeight
//PHeight 183.
//PHeight 3000.
Array.map (fun x -> x, P muHeight stdDevHeight x) [|10. .. 1. .. 300.|] |> Array.maxBy snd

//что вероятнее?
let xs = Array.map (fun x -> Math.Log(x)) <| Array.map PHeight data2  
Array.sum xs

let ys = Array.map (fun x -> Math.Log(x)) <| Array.map PHeight data3
Array.sum ys

Array.sum xs > Array.sum ys


[|100. .. 1. .. 300.|] |> Array.map (fun x -> x, PHeight x) |> Chart.Line

[| 0.1 .. 0.05 .. 10.|] |> Array.map (fun x -> x, Math.Log(x)) |> Chart.Line
