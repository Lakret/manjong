module Sergey //это не надо отправлять в интерпретатор

open System

//вот простенькая функция для генерации массивов с большим количеством повторов
let generateRandomArray length max =
    let rnd = new Random()
    [| for _ in 1..length -> rnd.Next(max) + 1 |]
    
//пример использования
//generateRandomArray 100 5 

//это тестовая функция, с помощью неё ты сможешь узнать, верно ли твоё решение
//она умеет делать то, что должен научиться делать ты, но с помощью функциональной,
//а не императивной части F#. Через 2 занятия этот код станет для тебя понятным,
//но пока смотреть на него не нужно
let test solution =
    let arr = generateRandomArray 1000 10
    let buffer = new System.Collections.Generic.Dictionary<_, _>()
    for elem in arr do
        if buffer.ContainsKey elem then 
            buffer.[elem] <- buffer.[elem] + 1
        else 
            buffer.Add(elem, 1)
    let expectedElem, expectedCount = Seq.maxBy snd [| for kvp in buffer -> kvp.Key, kvp.Value |]
    printfn "Ожидается мода %A с количеством %i" expectedElem expectedCount
    let (actualElem, actualCount) = solution arr
    if expectedElem = actualElem && expectedCount = actualCount then
        printfn "Верный ответ"
    else
        printfn "Неверный ответ (мода %A, количество %i)" actualElem actualCount

//когда напишешь код findMode, раскомменть и отправь то, что ниже, в интерпретатор: 
//test findMode

//тебе нужно написать эту функцию
//подумай, какой тип должен быть у этой функции?
let findMode arr = 
    //тут должен быть твой код
    //код, принадлежащий функции, обозначается отступом
    //функция возвращает то, что стоит в последней строчке её кода

    //в своём коде ты можешь использовать только:
    //Array.fold (тебе потребуются аж 2!), List.exists, List.maxBy, кортежи,
    //comprehensions, if'ы и даже (::)
    