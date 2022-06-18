module Graphs.ShortestPath

open System
open Microsoft.FSharp.Collections
open NUnit.Framework
open FsUnit

let edges: Edge<string> list =
    [ ("w", "x")
      ("x", "y")
      ("z", "y")
      ("z", "v")
      ("w", "v") ]

let shortestPath a b (graph: Graph<'a>) =
    let rec traverse queue visited =
        match queue with
        | (current, path) :: rest ->
            if current = b then
                path
            else
                let visited = Set.add current visited
                let edges = graph.[current]

                let notVisited =
                    Set.difference edges visited
                    |> Set.map (fun n -> (n, path + 1))
                    |> Set.toList 

                traverse (rest @ notVisited) visited
        | [] -> failwithf "Node %A not found!" b

    traverse [ (a, 0) ] Set.empty

[<Test>]
let Shortest_path () =
    edges
    |> Graph.fromEdges
    |> shortestPath "w" "z"
    |> should equal 2

[<Test>]
let Shortest_path_fail () =
    (fun () ->
        edges
        |> Graph.fromEdges
        |> shortestPath "w" "b"
        |> ignore)
    |> should (throwWithMessage "Node \"b\" not found!") typeof<Exception>
