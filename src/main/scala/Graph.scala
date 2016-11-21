/**
  * Created by timdelisle on 10/23/16.
  */
import javax.swing.text.Utilities

import collection.mutable

trait GraphLike {
  val nodes: Set[Node]
  val getEdges: Map[(Int, Int), Edge]

  var neighborhoods: Map[Int, Set[Node]]
  def addVertex(v: Node) = new Graph( nodes + v ,getEdges)
  def path(from: Node, to: Node) = GraphOps.breadthFirstSearch(this, from, to)
  def getEdge(to: Int, from: Int) = getEdges.get(to, from)

  def getNode(id: Int) = nodes.find(_.id == id)
  def removeEdge(to: Int, from: Int) = new Graph(nodes, getEdges - ((to,from)))
}


class Graph(vertices: Set[Node], edges: Map[(Int, Int), Edge]) extends GraphLike {
  val nodes: Set[Node] = vertices ++ edges.map(_._2.to) ++ edges.map(_._2.from)
  val getEdges = edges
  var neighborhoods: Map[Int, Set[Node]] = edges.values.toList.groupBy(_.from.id).mapValues[Set[Node]](x => x.map(_.to).toSet)
}


class BipartiteGraph(x: Set[Node], y: Set[Node], edges: Map[(Int, Int), Edge]) extends GraphLike{
  val nodes: Set[Node] = x ++ y ++ edges.map(_._2.to) ++ edges.map(_._2.from)
  val getEdges = edges
  var neighborhoods: Map[Int, Set[Node]] = edges.values.toList.groupBy(_.from.id).mapValues[Set[Node]](x => x.map(_.to).toSet)
}


class Matching(buyers: Map[Node, Map[Node, Float]], items: Map[Node, Float], edges: Map[(Int, Int), Edge]) extends GraphLike {
  val nodes: Set[Node] = buyers.keySet ++ items.keySet ++ edges.map(_._2.to) ++ edges.map(_._2.from)
  val getEdges = edges
  var neighborhoods: Map[Int, Set[Node]] = edges.values.toList.groupBy(_.from.id).mapValues[Set[Node]](x => x.map(_.to).toSet)

  // Adds a source and target node to the bypartite graph
  def getAugmentedGraph: Matching = {
    def makeInfCapacityEdges(edges: Map[(Int,Int), Edge]): Map[(Int,Int), Edge] = {
      edges.map(e => e._1 -> e._2.setCap(Int.MaxValue))
        .flatMap(n => List(n, n._2.ref.swap -> n._2.getReverse))
    }

    def genDirectedEdges(edges: Map[(Int,Int), Edge]): Map[(Int,Int), Edge] = {
      edges.flatMap(n => List(n, n._2.ref.swap -> n._2.getReverse))
    }

    // Gen edges from source to buyers
    // Capacity is one fo reach of these edges
    val edgeCap = 1
    val sourceNode = Node(0)
    val buyerEdges = this.buyers
      .map(n =>  Edge(sourceNode, n._1, edgeCap, 1.0))
      .map(n => n.ref -> n)
      .toMap

    // Gen Edges from items to target
    val targetNode = Node(10)
    val sellerEdges: Map[(Int, Int), Edge] = this.items
      .map(n =>  Edge(n._1, targetNode, edgeCap, 1.0))
      .map(n => n.ref -> n)
      .toMap
    new Matching(
      this.buyers,
      this.items,
      genDirectedEdges(makeInfCapacityEdges(edges)) ++ buyerEdges ++ sellerEdges
    )
  }

  // Substract the price from the utility for the item
  def getUtility(buyer: Node, item: Node) = buyers(buyer)(item) - items(item)
  def getPreferredChoice(buyer: Node): Node = {
    val items = buyers(buyer).keySet
    val utilities = items.map(i => (i, getUtility(buyer, i))).toSeq
    utilities.maxBy(_._2)._1
  }

  def acceptabilityGraph: Matching = {
    val acceptable = edges.filter(e =>  {
      val buyer = this.getNode(e._1._1)
      val item =  this.getNode(e._1._2)
      val utility = getUtility(buyer.get, item.get)
      utility >= 0.0
    })

    new Matching(buyers, items, acceptable)
  }

  def inducedPreferredGraph: Matching = {
    val preferred = edges.filter(e =>  {
      val buyer = this.getNode(e._1._1).get
      val preferredItem = getPreferredChoice(buyer)

      (e._2.from == buyer) && (e._2.to == preferredItem)
    })

    new Matching(buyers, items, preferred)
  }
}


case class Node(id: Int)
case class Edge(from: Node, to: Node, capacity: Int, weight: Double) {
  val ref = (from.id, to.id)
  def setCap(cap: Int) = {
    Edge(from, to, cap, weight)
  }
  def getReverse = {
    Edge(to, from, capacity, weight)
  }
}
case class Path(nodes: List[Int], distance: Double)
