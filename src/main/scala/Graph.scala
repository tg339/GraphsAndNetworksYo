/**
  * Created by timdelisle on 10/23/16.
  */
class Graph(vertices: Set[Node], edges: Set[Edge]) {
  val nodes: Set[Node] = vertices ++ edges.map(_.to) ++ edges.map(_.from)

  var neighborhoods: Map[Int, Set[Node]] = edges.groupBy(_.from.id).mapValues(x => x.map(_.to))

  def addEdge(e: Edge) = new Graph(nodes + e.to + e.from, edges + e)
  def addVertex(v: Node) = new Graph( nodes + v ,edges)
  def getEdge(to: Int, from: Int) = edges.find(x => x.to.id == to && x.from.id == from)

  def path(from: Node, to: Node) = GraphOps.beadthFirstSearch(this, from, to)
}

case class Node(id: Int, var visited: Boolean=false)
case class Edge(from: Node, to: Node, capacity: Int, weight: Double)
case class Path(nodes: List[Int], distance: Double)
