module Program

open System

//задание именованных значений
let a = 300   //целое число, тип: integer
let b = 500.5 //вещественное число, тип: float
let str = "This is BlackJack!" //строка, тип: string
let delimiter = ' ' //символ, тип: char
let f = (fun x -> x * 2) //функция, тип: int -> int

let g f x = f (f (f x))
g (fun x -> x * 2) 5 // = 40

let g x y = x * y //другая запись функции
let g' = g 20     //частичное применение

Array.iter (printfn "-- %c") //функция из модуля Array
<| str.ToCharArray()

[| 0..100 |] |> Array.min //минимальный элемент массива
|> ignore //чтобы компилятор не ругался ^_^

let gen = new Random() //новый генератор случайных чисел

[| for _ in 0..100 -> gen.NextDouble() * 100.0 - 50.0 |] //массив случайных чисел, array comprehension
//другой способ найти минимальный элемент
|> Array.fold (fun acc elem -> if elem < acc then elem else acc) Double.PositiveInfinity
|> ignore

//самодельный fold для списка
let rec fold f init list = //rec потому что функция рекурсивная
    match list with //pattern matching
    | [] -> init //если пустой список — возвращаем аккумулятор
    | x::xs -> fold f (f init x) xs //иначе применяем f к голове и тек. аккумулятору и продолжаем для хвоста

[ for _ in 0..100 -> gen.NextDouble() * 100. - 30. ] //list comprehension
|> fold (fun (min, max) elem -> (if elem < min then elem else min), (if elem > max then elem else max))
         (Double.PositiveInfinity, Double.NegativeInfinity)
||> printfn "Min: %f, Max: %f" // а про магический оператор ||> мы поговорим в след. среду :)

//самодельный fold для массива
let rec fold' f init arr =
    match arr with
    | [||] -> init
    | _ -> fold' f (f init arr.[0]) arr.[1..]

fold' (+) 0 [|1..100|] //сумма чисел через fold
|> ignore
