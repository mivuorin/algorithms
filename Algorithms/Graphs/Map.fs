module Graphs.Map

open NUnit.Framework
open FsUnit


[<Test>]
let Map_over_simple_graph () =
    Graph.create [ (3, []) ]
    |> Graph.map 3 id
    |> should equal [ 3 ]

[<Test>]
let Map_over_cyclic_graph () =
    Graph.create [ (1, [ 2 ]); (2, [ 1 ]) ]
    |> Graph.map 1 id
    |> should equivalent [ 1; 2 ]

[<Test>]
let Map_over_cyclic_graph_bug () =
    Graph.create [ (0, [ 8; 1; 5 ])
                   (1, [ 0 ])
                   (5, [ 0; 8 ])
                   (8, [ 0; 5 ]) ]
    |> Graph.map 8 id
    |> should equivalent [ 8; 0; 5; 1 ]
