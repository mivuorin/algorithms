module Graphs.GraphTraversal

open NUnit.Framework
open FsUnit

let a = "a"
let b = "b"
let c = "c"
let d = "d"
let e = "e"
let f = "f"

// adjacency list
let graph =
    Graph.create [ (a, [ b; c ])
                   (b, [ d ])
                   (c, [ e ])
                   (d, [ f ])
                   (e, [])
                   (f, []) ]

let rec depthFirst current graph =
    seq {
        yield current

        for next in Graph.edges current graph do
            yield! depthFirst next graph
    }

let depthFirst2 current graph =
    let rec loop stack items =
        match stack with
        | n :: rest ->
            let edges = Graph.edges n graph
            loop (edges @ rest) (n :: items)
        | [] -> items

    loop [ current ] [] |> List.rev

let breadthFirst current graph =
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
