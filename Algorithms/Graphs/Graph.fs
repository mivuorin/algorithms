namespace Graphs


type Graph<'a when 'a: comparison> = Map<'a, Set<'a>>

type Edge<'a> = 'a * 'a

module Graph =

    let create (adjacentList: seq<'a * 'a list>) =
        adjacentList
        |> Seq.map (fun (key, nodes) -> (key, Set nodes))
        |> Map

    let nodes graph = Map.keys graph |> List.ofSeq

    let edges node (graph: Graph<'a>) = graph.[node] |> Set.toList

    let notVisited node (visited: Set<'a>) (graph: Graph<'a>) =
        graph.[node] - visited |> Set.toList

    let fromEdges (edges: ('a * 'a) list) =
        let addOrPrepend node (existing: Set<'a> option) =
            match existing with
            | Some existing -> Set.add node existing |> Some
            | None -> Set [ node ] |> Some

        let rec loop edges graph =
            match edges with
            | (source, destination) :: rest ->
                graph
                |> Map.change source (addOrPrepend destination)
                |> Map.change destination (addOrPrepend source)
                |> loop rest
            | [] -> graph

        loop edges (Map [])

    let map start (f: 'a -> 'b) (graph: Graph<'a>) =
        let rec loop nodes results visited =
            match nodes with
            | current :: rest ->
                if Set.contains current visited then
                    loop rest results visited
                else
                    let result = f current

                    loop
                        (graph |> edges current |> List.append rest)
                        (result :: results)
                        (Set.add current visited)
            | [] -> results

        loop [ start ] [] Set.empty
