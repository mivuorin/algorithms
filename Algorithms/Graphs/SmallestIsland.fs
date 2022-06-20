module Graphs.SmallestIsland

open FSharp.Core
open NUnit.Framework
open FsUnit

let W = Exclude "W"
let L = Include "L"

let grid =
    array2D [ [ W; L; W; W; L; W ]
              [ L; L; W; W; L; W ]
              [ W; L; W; W; W; W ]
              [ W; W; W; L; L; W ]
              [ W; W; W; L; L; W ]
              [ W; W; W; L; W; W ] ]


let smallestIsland =
    Grid.components
    >> List.map List.length
    >> List.min

[<Test>]
let Smallest_island_size () =
    grid |> smallestIsland |> should equal 2
