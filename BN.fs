open System
open System.Collections.Generic

type Node = string

type Graph<'Node> = list<'Node * 'Node list>

let parents graph target =
    [
        //для каждого элемента списка смежностей
        for (node, childs) in graph do            
        //если в списке детей есть target
            if Seq.exists ((=) target) childs then
                //добавить эту ноду в результирующий список
                yield node        
    ]

let testG = [ ("A", ["B"; "D"]); ("B", []); ("C", ["D"]); ("D", []); ]
//parents testG "D"

let childs graph target = 
    List.filter (fun (node, _) -> node = target) graph
    |> List.head
    |> snd
    
//childs testG "A"
