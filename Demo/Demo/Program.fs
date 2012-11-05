module Program

open System
open System.Collections.Generic

//задание именованных значений
let a = 300   //целое число, тип: integer
let b = 500.5 //вещественное число, тип: float
let str = "This is BlackJack!" //строка, тип: string
let delimiter = ' ' //символ, тип: char
let f = (fun x -> x * 2) //функция, тип: int -> int

let g f x = f (f (f x))
g (fun x -> x * 2) 5 // = 40
|> ignore

//let g x y = x * y //другая запись функции
//let g' = g 20     //частичное применение

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


// Алгебраические типы данных

//соответствует встроенному в язык типу 'a Option
type 'a Maybe = //'a — переменная типа, Maybe — конструктор типа
    //конструкторы переменных
    | NoValue
    | Value of 'a //параметризованный конструктор переменных

//пример значений такого типа:
Value "slow" //: string Maybe
|> ignore
NoValue //:'a Maybe
|> ignore
(NoValue : int Maybe) //явно указываем тип
|> ignore

//тип этой функции — key:'a -> list:('a * 'b) list -> 'b Maybe when 'a : equality
//т.е.: для любого типа 'a с операцией сравнения на равенство, она принимает key этого типа,
//список кортежей типа ('a * 'b) и возвращает значение типа 'b Maybe
let rec findInList key list = //эта функция рекурсивна, поэтому нужно указать это с помощью rec
    match list with //pattern-matching
    | (k, v)::_ when k = key -> Value v //guard
    | _::xs -> findInList key xs
    | [] -> NoValue

findInList "secret" [ ("foo", 12); ("secret", 18); ("bar", 19) ] //Value 18 : int Maybe
|> ignore
findInList "secret" [ ("foo", 12); ("notASecret", 13); ("bar", 19) ] //NoValue : int Maybe
|> ignore

//та же функция, только для стандартного типа 'a Option
//посмотрите на её тип в интерпретаторе!
let rec findInList' key list = //эта функция рекурсивна, поэтому нужно указать это с помощью rec
    match list with
    | (k, v)::_ when k = key -> Some v
    | _::xs -> findInList' key xs
    | [] -> None


//подключаем все нужные библиотеки для работы с провайдерами типов
//и построения графиков
#if INTERACTIVE
#r "System.Data.Services.Client"
#r "FSharp.Data.TypeProviders"
#r @"..\packages\MSDN.FSharpChart.dll.0.60\lib\MSDN.FSharpChart.dll"
#load "FSharpChart.fsx"
#endif
open Microsoft.FSharp.Data.TypeProviders
open System.Linq
open MSDN.FSharp.Charting
open System.Drawing

//используем провайдер типов, чтобы получить данные StackOverflow
type stackOverflowData = ODataService<"http://data.stackexchange.com/stackoverflow/atom">
let context = stackOverflowData.GetDataContext()

//выбираем нужных пользователей
let users = context.Users 
            |> Seq.filter (fun user -> user.Age.HasValue) //которые указали возраст
            |> Seq.map (fun user -> user.DisplayName, float user.Age.Value) //преобразуем в нужный формат
            |> Seq.toArray //форсируем вычисление

let averageAge = Array.sumBy snd users / (float users.Length)

let hist data = Seq.countBy id data |> Seq.toArray

Array.map snd users |> hist |> FSharpChart.Column
|> ignore 