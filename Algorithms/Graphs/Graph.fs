namespace Graphs

type Node = string

type Edge = (Node * Node)

type Edges = Set<Node>

type Graph = Map<Node, Set<Node>>

module Graph =

    let create (adjacentList: seq<Node * Node list>) : Graph =
        adjacentList
        |> Seq.map (fun (key, nodes) -> (key, Set nodes))
        |> Map

    let empty: Graph = create []

    let node n = n

    let nodes values = values |> List.map node

    let edges node (graph: Graph) = graph.[node] |> Set.toList
    
    let notVisited node (visited: Edges) (graph: Graph) =
        graph.[node] - visited |> Set.toList

    let fromEdges (edges: Edge list) =
        let addOrPrepend node (existing: Edges option) =
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

        loop edges empty
