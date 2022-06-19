module Graphs.IslandCount

open NUnit.Framework
open FsUnit


let W = "W"
let L = "L"

let grid =
    [ [ W; L; W; W; L; W ]
      [ L; L; W; W; L; W ]
      [ W; L; W; W; W; W ]
      [ W; W; W; L; L; W ]
      [ W; L; W; L; L; W ]
      [ W; W; W; W; W; W ] ]

/// Returns next not visited edges of node in x y
let edges x y visited grid =
    let step (stepX, stepY) =
        let nextX = x + stepX
        let nextY = y + stepY

        if nextX < 0 || nextX >= List.length grid then
            None
        else if nextY < 0 || nextY >= List.length grid.[nextX] then
            None
        else
            let node = grid.[nextX].[nextY]

            if node = L
               && not <| Set.contains (nextX, nextY, node) visited then
                Some(nextX, nextY, node)
            else
                None

    let steps = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
    steps |> List.map step |> List.choose id


let islesCount grid =
    // iterate whole grid
    // when hit which is not visited land start traversing it's edges
    // mark all traversed nodes visited
    let loop (count, visited) (r, c, node) =
        if Set.contains (r, c, node) visited then
            (count, visited)
        else if node = W then
            (count, Set.add (r, c, node) visited)
        else
            // traverse land and mark it's nodes visited
            let rec traverseLand nodes visited =
                match nodes with
                | (x, y, current) :: rest ->
                    let visited = Set.add (x, y, current) visited
                    let edges = edges x y visited grid
                    traverseLand (rest @ edges) visited
                | [] -> visited

            let visited = traverseLand [ (r, c, node) ] visited
            (count + 1, visited)

    let count, _ =
        grid
        |> List.mapi
            (fun r row -> row |> List.mapi (fun c node -> (r, c, node)))
        |> List.concat
        |> List.fold loop (0, Set.empty)

    count

[<Test>]
let Isles_count () = islesCount grid |> should equal 4
