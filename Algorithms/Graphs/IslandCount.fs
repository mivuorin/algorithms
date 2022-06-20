module Graphs.IslandCount

open NUnit.Framework
open FsUnit

let W = Exclude "W"
let L = Include "L"

let grid =
    array2D [ [ W; L; W; W; L; W ]
              [ L; L; W; W; L; W ]
              [ W; L; W; W; W; W ]
              [ W; W; W; L; L; W ]
              [ W; L; W; L; L; W ]
              [ W; W; W; W; W; W ] ]

let islesCount = Grid.components >> List.length

[<Test>]
let Isles_count () = islesCount grid |> should equal 4
