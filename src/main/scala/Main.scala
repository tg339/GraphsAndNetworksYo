import scala.collection.mutable
import scala.util.Random
import GraphOps.{maxFlowIteration, maxFlow, contagiousInfection}

object Main extends App {
  val g = GraphFactories.testGraph1
  // 10.a
//  println(GraphOps.getConstrictedSet(g, GraphOps.connectedComponents(g, g.getNode(0).get)))
  // 10.b
//  val marketEq = GraphOps.getMarketEquilibrium(g)
//  println(marketEq.items)
//
//  // 11.a
//  // 11.b
//  val vcgPrices = GraphOps.vcg(g)
//  println(vcgPrices.items)
//  // 11.c
//  val clarkPrices = GraphOps.clarkPivotRule(g)
//  println(clarkPrices.items)

  // 12.a and b
  val gFor12a = GraphFactories.graphFor12a
  val vcgPricesFor12a = GraphOps.getMarketEquilibrium(gFor12a)
  println(vcgPricesFor12a.items)

  // 12.c
  val gFor12c = GraphFactories.graphFor12c
  val vcgPricesFor12c = GraphOps.getMarketEquilibrium(gFor12c)
  println(vcgPricesFor12c.items)

}

object HW1 {
  //  val g = GraphFactories.fromNumberAndProb(1000, .5F)
  //  println(g.findShortestPathBFS())
  //
  //  println(GraphFactories.runTest(g, 10000))

  //  val testGraphs = (0.0 to 1.0 by .01).foreach( x => {
  //    println((x, GraphFactories.runTest(GraphFactories.fromNumberAndProb(1000,x.toFloat), 400)))
  //  })
}

object HW2 {
  val graph = GraphFactories.fromFile("/Users/timdelisle/Downloads/facebook_combined.txt")

  println("total number of edges in directed graph")
  println(graph.getEdges.size)
  //4.11.b
  val testGraphs = GraphFactories.simpleTestGraphs
  val startNode: Int = Random.nextInt(graph.nodes.size)
  val endNode: Int = Random.nextInt(graph.nodes.size)

  println("test max flow:")
  println(testGraphs.map(maxFlow(_, graph.nodes.find(_.id == 0).get, graph.nodes.find(_.id == 3).get))) // 2, 2

  //4.11.c
  // Can set to maxInt 1000 if you've got a few minutes
  val maxInt = 100
  var totalEdgeDisjoints = 0
  (0 to maxInt).par.foreach(x => {
    val graphClone: Graph = GraphFactories.fromFile("/Users/timdelisle/Downloads/facebook_combined.txt")
    val startNode: Int = Random.nextInt(graphClone.nodes.size)
    val endNode: Int = Random.nextInt(graphClone.nodes.size)
    val maxFlowNumber = maxFlow(graphClone, graphClone.nodes.toArray.apply(startNode), graphClone.nodes.toArray.apply(endNode))
    totalEdgeDisjoints += maxFlowNumber._1
    println(maxFlowNumber)
  })
  println(s"Average for ${maxInt}: ")
  println(totalEdgeDisjoints/maxInt)

  //4.12.b
  val k = 10
  val q = 0.1

  println("infected | k | q")
  (0 to 100).par.foreach(x => {
    val infected = contagiousInfection(graph, k, q)
    println(infected.size, k, q)
  })
  //
  //  // 4.12.c
  val qs = 0.0 to 0.5 by .05
  val ks = 0 to 250 by 10

  val iterations = for { x <- qs; y <- ks } yield (x, y)

  println("infected | k | q")
  iterations.foreach(x => {
    val q = x._1
    val k = x._2

    val infected = contagiousInfection(graph, k, q)
    println(infected.size, k, q)
  })
}