module Graphs.UndirectedPath

open System
open FsUnit
open NUnit.Framework

let i = Graph.node "i"
let j = Graph.node "j"
let k = Graph.node "k"
let m = Graph.node "m"
let l = Graph.node "l"
let o = Graph.node "o"
let n = Graph.node "n"

let edges: Edge list =
    [ (i, j)
      (k, i)
      (m, k)
      (k, l)
      (o, n) ]

// unidirectional cyclic adjacent list
let expected =
    Graph.create [ (i, [ j; k ])
                   (j, [ i ])
                   (k, [ i; m; l ])
                   (m, [ k ])
                   (l, [ k ])
                   (o, [ n ])
                   (n, [ o ]) ]

let hasPath (source: Node) (destiny: Node) (graph: Graph) : bool =
    let rec loop nodes visited =
        match nodes with
        | current :: rest ->
            if current = destiny then
                true
            else if Set.contains current visited then
                false
            else
                let visited = Set.add current visited
                let next = Graph.notVisited current visited graph
                loop (next @ rest) visited
        | [] -> false

    loop [ source ] Set.empty

[<Test>]
let Edges_to_graph () =
    edges |> Graph.fromEdges |> should equal expected

[<Test>]
let HasPath_with_unidirectional_cyclic_graph () =
    edges
    |> Graph.fromEdges
    |> hasPath i m
    |> should equal true