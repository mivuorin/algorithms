module Graphs.ConnectedNodesCount

open FsUnit
open NUnit.Framework

let graph =
    Graph.create [ (3, [])
                   (4, [ 6 ])
                   (6, [ 4; 5; 7; 8 ])
                   (8, [ 6 ])
                   (7, [ 6 ])
                   (5, [ 6 ])
                   (1, [ 2 ])
                   (2, [ 1 ]) ]

let connectedNodeCounts (graph: Graph<'a>) =
    let rec traverse nodes visited =
        match nodes with
        | current :: rest ->
            let visited = Set.add current visited
            let next = Graph.notVisited current visited graph
            traverse (next @ rest) visited
        | [] -> visited

    let rec loop nodes count visited =
        match nodes with
        | current :: rest ->
            if Set.contains current visited then
                loop rest count visited
            else
                visited
                |> traverse [ current ]
                |> loop rest (count + 1)
        | [] -> count

    let nodes = Map.keys graph |> List.ofSeq
    loop nodes 0 Set.empty


[<Test>]
let Map_over_graph () =
    graph
    |> Graph.map 3 (fun n -> n)
    |> should equal [ 3 ]

[<Test>]
let Map_over_graph2 () =
    graph
    |> Graph.map 4 (fun n -> n)
    |> should equivalent [ 4; 5; 6; 7; 8 ]

[<Test>]
let Connected_nodes_count_in_graph () =
    graph |> connectedNodeCounts |> should equal 3

[<Test>]
let How_to_reduce () =
    [ 1; 2; 3 ]
    |> List.fold (fun state i -> 1 :: state) []
    |> should equal [ 1; 1; 1 ]
