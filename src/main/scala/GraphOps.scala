import util.Random
import collection.mutable
import util.control.Breaks._

object GraphOps {
  def backTrace(parents: mutable.Map[Int, Int], start: Node, end: Node) = {
    var path = List(end.id)
    while(path.head != start.id){
      path = parents(path.head)::path
    }
    path
  }


  def breadthFirstSearch(graph: GraphLike, startNode: Node, targetNode: Node): List[Int] = {
    val parents: mutable.Map[Int, Int] = mutable.Map.empty
    val q: mutable.Queue[Node] = new mutable.Queue
    var path: List[Int] = List()
    val visited: mutable.Set[Int] = mutable.Set().empty

    q.enqueue(startNode)
    if(startNode == targetNode) path = List(startNode.id, targetNode.id)
    else {
      breakable {
        while (q.nonEmpty) {
          val node = q.dequeue()
          visited.add(node.id)
          graph.neighborhoods.get(node.id) match {
            case Some(neighbors) => neighbors.foreach(neighbor => {
              if(!visited.contains(neighbor.id)) {
                parents(neighbor.id) = node.id
                visited.add(neighbor.id)
                if(neighbor.id == targetNode.id) {
                  path = backTrace(parents, startNode, targetNode)
                  break
                }
                q.enqueue(neighbor)
              }
            })
            case None => path = Nil; break
          }
        }
      }

    }
    path
  }

  // Can be used to find a constricted set
  def connectedComponents(graph: GraphLike, startNode: Node): mutable.Set[Int] = {
    val q: mutable.Queue[Node] = new mutable.Queue
    val visited: mutable.Set[Int] = mutable.Set().empty

    q.enqueue(startNode)

    breakable {
      while (q.nonEmpty) {
        val node = q.dequeue()
        visited.add(node.id)
        graph.neighborhoods.get(node.id) match {
          case Some(neighbors) => neighbors.foreach(neighbor => {
            if(!visited.contains(neighbor.id)) {
              visited.add(neighbor.id)
              q.enqueue(neighbor)
            }
          })
          case None => break
        }
      }
    }
    visited
  }

  def getMarketEquilibrium(graph: Matching): Matching = {
    // Set prices to zero
    var startingItems = graph.items.map(x => x._1 -> 0.0F)
    // Create new graph with new prices
    val startingGraph = new Matching(graph.buyers, startingItems, graph.getEdges)
    // Get graph with additional nodes to run maxflow
    var g = startingGraph.inducedPreferredGraph.getAugmentedGraph

    var maxFlow = GraphOps.maxFlow(g, g.getNode(0).get, g.getNode(10).get)
    // Do yo thang
    breakable {
      while(maxFlow._1 != startingItems.size) {
        // Get the constricted set
        val sellersConSet = GraphOps.getConstrictedSet(g, GraphOps.connectedComponents(g, g.getNode(0).get))
        val tmp = startingItems.filterNot(x => sellersConSet.keySet.contains(x._1))
        // Add one to all prices
        val updatedSellersConSet = sellersConSet.map(x => (x._1, x._2 + 1))
        startingItems = tmp ++ updatedSellersConSet

        // If all prices are greater than zero decrease by one
        if(startingItems.count(_._2 > 0) == startingItems.size) {
          startingItems = startingItems.map(i => (i._1, i._2 - 1 ))
        }

        // Output the actual graph
        val tmpG = new Matching(graph.buyers, startingItems, graph.getEdges)
        g = tmpG.inducedPreferredGraph.getAugmentedGraph
        maxFlow = GraphOps.maxFlow(g, g.getNode(0).get, g.getNode(10).get)
      }
    }
    new Matching(graph.buyers, startingItems, graph.getEdges)
  }

  def vcg(g: Matching) = {
    // Get the optimal social assignment through market equilibirum
    val marketEq = GraphOps.getMarketEquilibrium(g).inducedPreferredGraph
    val prices = marketEq.items

    val newPrices = prices.map( p => {
      val buyerNodeId = marketEq.getEdges.find(_._1._2 == p._1.id).get._1._1
      val otherValuationsSum = marketEq.getEdges.filterNot(_._1._1 == buyerNodeId).map(e =>{
        val chosenItemId = e._1._2
        val currentBuyerValuations = marketEq.buyers(marketEq.getNode(e._1._1).get)
        currentBuyerValuations(marketEq.getNode(chosenItemId).get)
      }).sum
      (p._1, otherValuationsSum * -1)
    })
    new Matching(g.buyers, newPrices, g.getEdges)
  }


  def clarkPivotRule(g: Matching): Matching = {
    // Are we converging to seller optimal market clearing prices?!?!?!?!?!?
    val seedGraph = GraphOps.getMarketEquilibrium(g)
    // Set all prices to zero but keep the matchings so that induced prefered uses the same price for all items
    val seedItems = seedGraph.items.map(x => (x._1, 0.0F))
    val marketEq = new Matching(seedGraph.buyers, seedItems, seedGraph.getEdges)

    val prices = marketEq.inducedPreferredGraph.items

    val globalWinningVals = prices.map( p => {
      val itemNode = p._1
      val buyerNode = seedGraph.getBuyerFromSeller(itemNode.id)
      val valuation = seedGraph.getValuations(buyerNode.id)(itemNode)
      (buyerNode, valuation)
    })



    val newPrices = prices.map(b => {
      val itemNode = b._1
      println("itemnode")
      println(itemNode.id)
      val buyerNode = seedGraph.inducedPreferredGraph.getBuyerFromSeller(itemNode.id)
      val newGraph = marketEq.removeNode(buyerNode.id).inducedPreferredGraph.breakTie
      println("Without Node " + buyerNode.id + " :")
      println(newGraph.getEdges)
      val newGraphPrices: Map[Node, Float] = newGraph.items.filter(x => newGraph.getEdges.keySet.flatMap(x => List(x._1, x._2)).contains(x._1.id))

      val localWinningVals = newGraphPrices.map( p => {
        val localItemNode = p._1
        val newBuyerNode = newGraph.getNode(newGraph.getEdges.find(_._1._2 == localItemNode.id).get._1._1).get
        val valuation = newGraph.buyers(newBuyerNode)(localItemNode)
        (newBuyerNode, valuation)
      })
      println("Global Winning Vals:")
      println(globalWinningVals)
      println("Local Winning Vals")
      println(localWinningVals)

      (itemNode, localWinningVals.map(x => x._2 - globalWinningVals(x._1)).sum)
    })
    println(newPrices)

    new Matching(g.buyers, newPrices, g.getEdges)

  }

  def getConstrictedSet(graph: Matching, connectedComponent: mutable.Set[Int]) = {
    graph.items.filter(s => connectedComponent.contains(s._1.id))
  }

  def maxFlow(graph: GraphLike, startNode: Node, targetNode: Node): (Int, Option[GraphLike]) = {
    var maxFlow = 0
    var g: Option[GraphLike] = Some(graph)

    while(g.isDefined) {
      val graphMinCPair = maxFlowIteration(g.get, startNode, targetNode)
      graphMinCPair match {
        case Some(x) =>
          g = Some(x._1)
          maxFlow += x._2
        case None => g = None
      }
    }
    (maxFlow, g)
  }

  def maxFlowIteration(graph: GraphLike, startNode: Node, targetNode: Node): Option[(GraphLike, Int)] = {
    // Find a path
    val pathSlider = graph.path(startNode, targetNode).sliding(2)
    val path = pathSlider.map(x => graph.getEdge(x.head, x.last).get).toList

    path match {
      case Nil => None
      case _ =>
        // Get max flow of the path by finding the bottleneck
        val minCap = path.minBy(_.capacity).capacity
        // Get the edges and remove the ones on the path
        var edges = graph.getEdges
        path.foreach(e => edges = graph.getEdges - ((e.from.id, e.to.id)))

        // List to add new nodes that might need to be created
        var pathAugmentation: Set[Edge] = Set()

        // For each edge in the path reduce flow by min cap
        // Get the node reverse edge and increment its flow. If it doesn't exist create a new edge
        // Finally filter out any nodes with zero cap
        val newPath = path.map( edge => {
          val currentCap = edge.capacity
          graph.getEdge(edge.to.id, edge.from.id) match {
            case Some(x) => pathAugmentation = pathAugmentation + Edge(x.to, x.from, x.capacity + minCap, 1)
            case None => pathAugmentation = pathAugmentation + Edge(edge.to, edge.from, minCap, 1)
          }
          Edge(edge.from, edge.to, currentCap - minCap, 1)
        }).filter(_.capacity > 0)

        // Update the edges to include all new edges
        // Make sure not to duplicate the edges

        if(newPath.nonEmpty) {
          newPath.foreach(e => edges = edges + ((e.from.id, e.to.id) -> e))
        }
        if(pathAugmentation.nonEmpty) {
          pathAugmentation.foreach(e => edges = edges + ((e.from.id, e.to.id) -> e))
        }

        // Return the new graph
        Some((new Graph(graph.nodes, edges), minCap))
    }
  }

  def contagiousInfection(graph: GraphLike, numEarlyAdopters: Int, contagionThreshold: Double): mutable.Set[Int] = {
    // Create queue of early adopters
    val adopters = mutable.Set(Random.shuffle(graph.nodes.toSeq).take(numEarlyAdopters).map(_.id): _*)
    var adoptersIt = mutable.Set[Int]()
    val q: mutable.Queue[Int] = mutable.Queue(graph.nodes.toSeq.map(_.id): _*)
    val visited = mutable.Set[Int]()

    breakable {
      while(q.nonEmpty) {
        adoptersIt = adopters.clone()
        val node = q.dequeue()
        visited.add(node)
        var infectedNeighbors = 0
        val nodeNeighbors = graph.neighborhoods(node)
        nodeNeighbors.foreach(n => {
          q.enqueue(n.id)
          if(adopters.contains(n.id)) infectedNeighbors = infectedNeighbors + 1
        })
        if (infectedNeighbors.toDouble / nodeNeighbors.size >= contagionThreshold) adopters.add(node)
        if (adoptersIt == adopters) break
      }
    }
    adopters
  }
}


//def findShortestPathBFS(startNode: Node= Random.shuffle(nodes.toList).head, endNode: Node = Random.shuffle(nodes.toList).last): List[String] = {
//    val parents: mutable.Map[String, String] = mutable.Map.empty
//    val q: mutable.Queue[Node] = new mutable.Queue
//    var path: List[String] = List()
//    q.enqueue(startNode)
//    breakable {
//      while(q.nonEmpty) {
//        val node = q.dequeue()
//        if(node == endNode) {
//          path = backTrace(parents, startNode, endNode)
//          break
//        }
//        if(!node.visited) {
//          neighborhoods.getOrElse(node, mutable.Set.empty).filter(n => !n.visited && n.identifier != node.identifier).foreach(neighbor =>{
//            if(neighbor.identifier == endNode.identifier) {
//              parents(neighbor.identifier) = node.identifier
//              path = backTrace(parents, startNode, endNode)
//              break
//            } else {
//              parents(neighbor.identifier) = node.identifier
//              q.enqueue(neighbor)
//            }
//          })
//        }
//        node.visited = true
//      }
//
//    }
//    path
//  }