namespace Graphs

type Node = string
type Graph = Map<Node, Node list>


module Graph =
    let create adjacentList =
        Map adjacentList
    let node n = n
    
    let nodes values =
        values
        |> List.map node

    let edges node (graph: Graph) = graph.[node]
