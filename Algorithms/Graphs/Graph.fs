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

    /// Maps over linked nodes in graph
    let map start mapping graph =
        let rec loop nodes results visited =
            match nodes with
            | current :: rest ->
                if Set.contains current visited then
                    loop rest results visited
                else
                    let result = mapping current

                    loop
                        (graph |> edges current |> List.append rest)
                        (result :: results)
                        (Set.add current visited)
            | [] -> results

        loop [ start ] [] Set.empty

    /// Maps over all linked nodes in graph
    let mapMany mapping graph =
        let rec loop nodes visited results =
            match nodes with
            | current :: rest ->
                if Set.contains current visited then
                    loop rest visited results
                else
                    let traversed = map current id graph
                    let result = mapping (traversed)
                    loop rest (visited + Set(traversed)) (result :: results)
            | [] -> results

        loop (nodes graph) Set.empty []

    /// Tests if any node in linked nodes satisfies predicate
    let exists start predicate graph : bool =
        let rec loop nodes visited =
            match nodes with
            | current :: rest ->
                if Set.contains current visited then
                    loop rest visited
                else if predicate (current) then
                    true
                else
                    loop
                        (graph |> edges current |> List.append rest)
                        (Set.add current visited)
            | [] -> false

        loop [ start ] Set.empty

    /// Prints linked nodes in graph
    let print graph =
        graph |> mapMany (printfn "%A") |> ignore
