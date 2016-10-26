/**
  * Created by timdelisle on 10/23/16.
  */
class Graph(vertices: Set[Node], edges: Map[(Int, Int), Edge]) {
  val nodes: Set[Node] = vertices ++ edges.map(_._2.to) ++ edges.map(_._2.from)
  val getEdges = edges

  var neighborhoods: Map[Int, Set[Node]] = edges.values.toList.groupBy(_.from.id).mapValues[Set[Node]](x => x.map(_.to).toSet)

//  def addEdge(e: Edge) = new Graph(nodes + e.to + e.from, edges + e)

  def addVertex(v: Node) = new Graph( nodes + v ,edges)
  def getNode(id: Int) = nodes.find(_.id == id)

  def removeEdge(to: Int, from: Int) = new Graph(nodes, edges - ((to,from)))
  def getEdge(to: Int, from: Int) = edges.get(to, from)

  def path(from: Node, to: Node) = GraphOps.breadthFirstSearch(this, from, to)
}

case class Node(id: Int)
case class Edge(from: Node, to: Node, capacity: Int, weight: Double)
case class Path(nodes: List[Int], distance: Double)
