module Graphs.HasPathProblem

open FsUnit
open NUnit.Framework

let f = Graph.node "f"
let g = Graph.node "g"
let h = Graph.node "h"
let i = Graph.node "i"
let j = Graph.node "j"
let k = Graph.node "k"

// Directed acyclic graph
let graph =
    Graph.create [ (f, [ g; i ])
                   (g, [ h ])
                   (h, [])
                   (i, [ g; k ])
                   (j, [ i ])
                   (k, []) ]

let hasPath (source: 'a) (destiny: 'a) (graph: Graph<'a>) : bool =
    let rec loop nodes =
        match nodes with
        | current :: rest ->
            if current = destiny then
                true
            else
                loop (Graph.edges current graph @ rest)
        | [] -> false

    loop [ source ]

[<Test>]
let HasPath_between_f_and_h () =
    graph |> hasPath f h |> should equal true

[<Test>]
let HasPath_between_f_and_j () =
    graph |> hasPath f j |> should equal false
