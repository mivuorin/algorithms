namespace Graphs

type Node<'a> =
    | Include of 'a
    | Exclude of 'a

type Grid<'a> = Node<'a> [,]

module Grid =
    
    let private step x y visited grid (stepX, stepY)  =
        let maxY = Array2D.length1 grid
        let maxX = Array2D.length2 grid

        let nextX = x + stepX
        let nextY = y + stepY

        if nextX < 0 || nextX >= maxX then
            None
        else if nextY < 0 || nextY >= maxY then
            None
        else
            let node = Array2D.get grid nextX nextY

            if Set.contains (nextX, nextY, node) visited then
                None
            else
                match node with
                | Exclude _ -> None
                | Include _ -> Some(nextX, nextY, node)

    let private edges x y visited grid =
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> List.map (step x y visited grid)
        |> List.choose id

    let rec private traverse grid nodes visited results =
        match nodes with
        | (r, c, current) :: rest ->
            let visited = Set.add (r, c, current) visited
            let edges = edges r c visited grid
            let next = rest @ edges
            traverse grid next visited (current :: results)
        | [] -> (visited, results)

    let components grid =
        let maxRows = Array2D.length1 grid
        let maxColumns = Array2D.length2 grid

        let rec loop r c visited isles =
            if r >= maxRows then
                isles
            else if c >= maxColumns then
                loop (r + 1) 0 visited isles
            else
                let node = Array2D.get grid r c

                if Set.contains (r, c, node) visited then
                    loop r (c + 1) visited isles
                else
                    match node with
                    | Exclude _ -> loop r (c + 1) visited isles
                    | Include _ ->
                        // Node is included and not visited, traverse it
                        let visited, results =
                            traverse grid [ (r, c, node) ] visited []

                        loop r (c + 1) visited (results :: isles)

        loop 0 0 Set.empty []
