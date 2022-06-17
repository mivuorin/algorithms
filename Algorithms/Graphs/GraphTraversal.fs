module Graphs.GraphTraversal

open NUnit.Framework
open FsUnit

let a = Graph.node "a"
let b = Graph.node "b"
let c = Graph.node "c"
let d = Graph.node "d"
let e = Graph.node "e"
let f = Graph.node "f"

// adjacency list
let graph: Graph =
    Graph.create [ (a, [ b; c ])
                   (b, [ d ])
                   (c, [ e ])
                   (d, [ f ])
                   (e, [])
                   (f, []) ]

let rec depthFirst current (graph: Graph) =
    seq {
        yield current

        for next in Graph.edges current graph do
            yield! depthFirst next graph
    }

let depthFirst2 current (graph: Graph) =
    let rec loop stack items =
        match stack with
        | n :: rest ->
            let edges = Graph.edges n graph
            loop (edges @ rest) (n :: items)
        | [] -> items

    loop [ current ] [] |> List.rev

let breadthFirst current (graph: Graph) =
    let rec loop queue items =
        match queue with
        | n :: rest ->
            let edges = Graph.edges n graph
            loop (rest @ edges) (n :: items)
        | [] -> items

    loop [ current ] [] |> List.rev

[<Test>]
let Depth_first_nodes () =
    graph
    |> depthFirst a
    |> should equal [ a; b; d; f; c; e ]

[<Test>]
let Depth_first_nodes2 () =
    graph
    |> depthFirst2 a
    |> should equal [ a; b; d; f; c; e ]

[<Test>]
let Breadth_first_nodes () =
    graph
    |> breadthFirst a
    |> should equal [ a; b; c; d; e; f ]
