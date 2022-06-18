module Graphs.LargestComponent

open FsUnit
open NUnit.Framework

let graph =
    Graph.create [ (0, [ 8; 1; 5 ])
                   (1, [ 0 ])
                   (5, [ 0; 8 ])
                   (8, [ 0; 5 ])
                   (2, [ 3; 4 ])
                   (3, [ 2; 4 ])
                   (4, [ 3; 2 ]) ]

[<Test>]
let Pretty_print () = Graph.print graph

[<Test>]
let Largest_component_in_graph () =
    graph
        |> Graph.mapMany List.length
        |> List.max
    |> should equal 4