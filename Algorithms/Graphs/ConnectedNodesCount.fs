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
    graph |> Graph.mapMany (fun _ -> 1) |> List.sum

[<Test>]
let Connected_nodes_count_in_graph () =
    graph |> connectedNodeCounts |> should equal 3

[<Test>]
let How_to_reduce () =
    [ 1; 2; 3 ]
    |> List.fold (fun state i -> 1 :: state) []
    |> should equal [ 1; 1; 1 ]
