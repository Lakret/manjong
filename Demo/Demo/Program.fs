﻿module Program 

open System
open System.Collections.Generic

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
open System.IO

//ассоциативные массивы (они же словари или отображения)
let arr = [| ("hello", 0.25); ("kitty", 0.5); ("foo", 0.2); ("bar", 0.3) |]

//snd arr.[1]
//Array.find (fun x -> fst x = "kitty") arr |> snd

let d = arr |> dict //IDictionary<string,float>

d.["kitty"]
|> ignore

[|
    for i in 0..100 -> char i, i
|]
|> dict
|> ignore

//spell-checker

[|for i in 0..10000000 -> i|] |> ignore

let sequence = seq { for i in 0..10000000 -> i }

let testSet = [| 1; 2; 1; 3; 4|] |> set

testSet.Contains 1 |> ignore
testSet.Contains 454545 |> ignore

let delimiters = [| ' '; '\n'; '\r'; '='; '"'; '\''; '\t'; ','; '.'; ':'; ';'; '-'; '—'; '!'; '?' |]

let learnDictionary (file : string) =
    let words = (File.ReadAllText file).Split(delimiters, StringSplitOptions.RemoveEmptyEntries)
    let n = float words.Length
    Seq.countBy (fun (x : string) -> x.ToLower()) words 
    |> Seq.map (fun (word, count) -> word, (float count) / n)
    |> dict

let W = learnDictionary @"C:\Users\uc-user\Documents\manjong\Data\Bible_txt.txt"

//all words with Levenstein distance = 1 from the given word
//string -> Set<string>
//замена, вставка, удаление, обмен местами
let generateAllNeighbours (inputWord : string) (W : IDictionary<string, _>) =
    let word = inputWord.ToLower()
    let maxPos = word.Length - 1
    [|
        //deletions
        for i in 0..maxPos -> word.Remove(i, 1)
        //insertions and replaces
        for c in 'а'..'я' do
            yield word + c.ToString()
            for pos in 0..maxPos do
                yield word.Insert(pos, c.ToString())
                let charArray = word.ToCharArray()
                charArray.[pos] <- c
                yield new String(charArray)
        //exchanges
        for x in 0..maxPos do
            for y in 0..maxPos do
                let charArray = word.ToCharArray()
                let buffer = charArray.[x]
                charArray.[x] <- charArray.[y]
                charArray.[y] <- buffer
                yield new String(charArray)
    |]
    |> set
    |> Set.filter W.ContainsKey
    |> Set.map (fun word -> word, W.[word])

(generateAllNeighbours "привер" W)

let spellCheck (W : IDictionary<_, _>) (w : string) = 
    let word = w.ToLower()
    if W.ContainsKey word then word
    else 
        let neighbours = generateAllNeighbours word W
        if neighbours.IsEmpty then word
        else Seq.maxBy (fun (_, p) -> p) neighbours |> fst

let spellCheck' = spellCheck W

//spellCheck' "привер"
//spellCheck' "инфраструктура"

let text =
    "Когда мать увдела сныа, она крыкнула: хватай кошку и беги! Крета бысстро мчаа."

text.Split(delimiters, StringSplitOptions.RemoveEmptyEntries)
|> Array.map spellCheck'

//"яКрИвЕтКО".ToLower()






let dataDir = @"C:\Users\uc-user\Documents\manjong\Data"
Directory.GetFiles dataDir

let lines = (File.ReadAllLines (dataDir + @"\females_out_of_school.csv")).[1..]

let parseLine (line : string) =
    let splitted = line.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
    if splitted.Length <> 2 then None
    else Some (splitted.[0], float splitted.[1])

//Array.map parseLine lines
//|> Array.filter Option.isSome
//|> Array.map Option.get

let data =
    [|
        for line in lines do
            match parseLine line with
            | Some x -> yield x
            | _ -> ()
    |]
    |> Array.sortBy snd

data
|> Array.map (fun (x, y) -> x, Math.Log10 y)
|> FSharpChart.Column


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

//by Nikolai:
[|for x in 0.0..0.001..(2.0*Math.PI) -> (x, Math.Sin x)|]
|> FSharpChart.Point
|> ignore

// Алгебраические типы данных

type CustomBool =
    | True
    | False

let customAnd x y =
    match x, y with
    | True, True -> True
    | _ -> False 

customAnd True False
|> ignore
customAnd True True
|> ignore

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

//реализация списка
type 'a List =
    | Nil
    | Node of 'a * 'a List

//пример списка
let list = Node(12, Node(-3, Node(5, Nil))) //: int List

let rec sum =
    function
    | Nil -> 0
    | Node(x, tail) -> x + sum tail

printfn "Sum of elements of %A is %i" list <| sum list

//DSL
open Types //смотрите тип в модуле Types

//пример записи вышеприведённого уравнения
let testEq = Eq(Add(Mul(Int 2, Pow(Var "x", Int 2)), Add(Neg(Mul(Int 8, Var "x")), Int 15)), Int 20)

let solve (Eq(left, right)) =
    //мы должны извлечь обычные коэффициенты a, b и c
    let rec extractA =
        function 
        | Mul(Neg(Int a), Pow(Var _, Int 2)) -> Some -a
        | Mul(Int a, Pow(Var _, Int 2)) -> Some a
        | Pow(Var _, Int 2) -> Some 1
        | Add(x, y) -> 
            match extractA x with
            | Some a -> Some a
            | None -> extractA y
        | _ -> None
    
    let rec extractB =
        function
        | Mul(Int b, Var _) -> Some b
        | Neg(Mul(Int b, Var _)) -> Some -b
        | Add(x, y) -> 
            match extractB x with
            | Some b -> Some b
            | None -> extractB y
        | _ -> None

    let rec extractC =
        function
        | Neg(Add(left, right)) -> extractC <| Add(left, Neg(right))
        | Add(left, right) ->
            match extractC left with
            | Some c -> Some c
            | None -> extractC right
        | Int c -> Some c
        | Neg(Int c) -> Some -c
        | _ -> None

    let extractRight =
        function
        | Neg(Int x) -> x
        | Int x -> x
        | _ -> failwith "Incorrect right side of equation: only integers are allowed"

    let solver a b c =
        let a', b', c' = float a, float b, float c
        let d = Math.Sqrt(b' ** 2. - 4. * a' * c')
        (- b' + d / (2. * a')), (- b' - d / (2. * a'))

    match extractA left, extractB left, extractC left with
    | Some a, Some b, Some c -> 
        printfn "a = %i, b = %i, c = %i" a b c
        solver a b (c - extractRight right)
    | _ -> failwith "Incorrect input"

//используем лексер и парсер, чтобы научиться читать ввод пользователей
#if INTERACTIVE
#r "FSharp.PowerPack.dll"
#endif

[<EntryPoint>]
let main(_) =
    let eq = "-8 * x ** 2 + 132 * x + 18 = -5"
//    let lexbuf = Lexing.LexBuffer<_>.FromString eq
//    while not lexbuf.IsPastEndOfStream do
//        printfn "Lex: %A" <| QuadLexer.tokenize lexbuf
    let testEquation2 = QuadParser.start <| QuadLexer.tokenize <| Lexing.LexBuffer<_>.FromString eq
    printfn "Solved equation %A with answers %f and %f" testEquation2 <|| solve testEquation2
    0

