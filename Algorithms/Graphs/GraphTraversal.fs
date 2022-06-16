module Graphs.GraphTraversal

open NUnit.Framework
open FsUnit

type Node = string
type Graph = Map<Node, Node list>

let node n = n
let a = node "a"
let b = node "b"
let c = node "c"
let d = node "d"
let e = node "e"
let f = node "f"

// adjacency list
let graph: Graph =
    Map [ (a, [ b; c ])
          (b, [ d ])
          (c, [ e ])
          (d, [ f ])
          (e, [])
          (f, []) ]

let edges node (graph: Graph) = graph.[node]

let rec depthFirst current (graph: Graph) =
    seq {
        yield current
        for next in edges current graph do
            yield! depthFirst next graph
    }

let depthFirst2 current (graph: Graph) =
    let rec loop stack items =
        match stack with
        | n :: rest ->
            let edges = edges n graph
            loop (edges @ rest) (n :: items)
        | [] -> items

    loop [ current ] [] |> List.rev

let breadthFirst current (graph: Graph) =
    let rec loop queue items =
        match queue with
        | n :: rest ->
            let edges = edges n graph
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
