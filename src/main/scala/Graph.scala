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
  def removeNode(id: Int): GraphLike
}


class Graph(vertices: Set[Node], edges: Map[(Int, Int), Edge]) extends GraphLike {
  val nodes: Set[Node] = vertices ++ edges.map(_._2.to) ++ edges.map(_._2.from)
  val getEdges = edges
  var neighborhoods: Map[Int, Set[Node]] = edges.values.toList.groupBy(_.from.id).mapValues[Set[Node]](x => x.map(_.to).toSet)
  def removeNode(id: Int) = new Graph(
    nodes.filterNot(_.id == id),
    getEdges.filterNot(e => e._1._1 == id || e._1._2 == id)
  )
}


class BipartiteGraph(val x: Set[Node], val y: Set[Node], edges: Map[(Int, Int), Edge]) extends GraphLike{
  val nodes: Set[Node] = x ++ y ++ edges.map(_._2.to) ++ edges.map(_._2.from)
  val getEdges = edges
  var neighborhoods: Map[Int, Set[Node]] = edges.values.toList.groupBy(_.from.id).mapValues[Set[Node]](x => x.map(_.to).toSet)
  def removeNode(id: Int) = new BipartiteGraph(
    x.filterNot(_.id == id),
    y.filterNot(_.id == id),
    getEdges.filterNot(e => e._1._1 == id || e._1._2 == id)
  )
}


class Matching(val buyers: Map[Node, Map[Node, Float]], val items: Map[Node, Float], edges: Map[(Int, Int), Edge]) extends GraphLike {
  val nodes: Set[Node] = buyers.keySet ++ items.keySet ++ edges.map(_._2.to) ++ edges.map(_._2.from)
  val getEdges = edges
  var neighborhoods: Map[Int, Set[Node]] = edges.values.toList.groupBy(_.from.id).mapValues[Set[Node]](x => x.map(_.to).toSet)

  def getValuations(buyerNodeId: Int): Map[Node, Float] = {
    val buyerNode = getNode(buyerNodeId).get
    buyers(buyerNode)
  }

  def getBuyersFromSeller(sellerId: Int): Seq[Node] = {
    edges.filter(_._1._2 == sellerId).map(x => this.getNode(x._1._1).get).toSeq
  }

  def getBuyerFromSeller(sellerId: Int): Node = {

    inducedPreferredGraph.getEdges.filter(_._1._2 == sellerId).map(x => getNode(x._1._1).get).toSeq.head
  }

  def removeNode(id: Int): Matching = new Matching(
    buyers.filterNot(_._1.id == id).map(x => (x._1, x._2.filterNot(_._1.id == id))),
    items.filterNot(_._1.id == id),
    getEdges.filterNot(e => e._1._1 == id || e._1._2 == id)
  )

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

  // Subtract the price from the utility for the item
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

  def breakTie: Matching = {
    val edgeGroups = edges.groupBy(_._1._2).filter(_._2.size > 1)
    val itemBuyers = edgeGroups.map(x => (x._1, getBuyersFromSeller(x._1)))
    val buyerNodeEdges: Seq[Seq[((Int, Int),Edge)]] = itemBuyers.map(bs => {
      val sellerNodeId = bs._1
      val buyerNodeVals = bs._2.map(n => (n.id, getValuations(n.id)(getNode(sellerNodeId).get)))
      val maxValBuyerId = buyerNodeVals.maxBy(_._2)._1
      val otherBuyerNodes = buyerNodeVals.filterNot(_._1 == maxValBuyerId)
      val newEdges = otherBuyerNodes.map(n => {
        val buyerNodeId = n._1
        val nextHighestChoice = getValuations(buyerNodeId).filterNot(_._1.id == sellerNodeId).maxBy(_._2)
        (buyerNodeId, nextHighestChoice._1.id) -> Edge(getNode(buyerNodeId).get, getNode(nextHighestChoice._1.id).get, 0, 0.0)
      })
      newEdges
    }).toSeq

    val setTiedBuyers = buyerNodeEdges.flatten.map(x => x._1._1).toSet

    new Matching(buyers, items, edges.filterNot(e => setTiedBuyers.contains(e._1._1)) ++ buyerNodeEdges.flatten)
  }

//  def inducedTopBidPriceGraph: Matching = {
//    val preferred = edges.filter(e =>  {
//      val buyer = this.getNode(e._1._1).get
//      val preferredItem = getPreferredChoice(buyer)
//
//      (e._2.from == buyer) && (e._2.to == preferredItem)
//    })
//    items.map(x => {
//      val buyerNodeId = preferred.find(_._1._2 == x._1.id).get._1._1
//      val preferredPrice = buyers(this.getNode(buyerNodeId).get)
//
//      preferredPrice
//    })
//
//    new Matching(buyers, items, preferred)
//  }
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
