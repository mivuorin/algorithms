module Graphs.ConnectedNodesCount

open FsUnit
open NUnit.Framework

let graph =
    Graph.create [ (3, [])
                   (4, [ 6 ])
                   (6,
                    [ 4
                      5
                      7
                      8 ])
                   (8, [ 6 ])
                   (7, [ 6 ])
                   (5, [ 6 ])
                   (1, [ 2 ])
                   (2, [ 1 ]) ]
