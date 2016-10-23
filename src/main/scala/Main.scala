import scala.collection.mutable
import scala.util.Random

object Main extends App {
//  val g = GraphFactories.fromNumberAndProb(1000, .5F)
//  println(g.findShortestPathBFS())
//
//  println(GraphFactories.runTest(g, 10000))

//  val testGraphs = (0.0 to 1.0 by .01).foreach( x => {
//    println((x, GraphFactories.runTest(GraphFactories.fromNumberAndProb(1000,x.toFloat), 400)))
//  })
  val graph = GraphFactories.fromFile("/Users/timdelisle/Downloads/facebook_combined.txt")
  graph.path(Random.shuffle(graph.nodes).head, Random.shuffle(graph.nodes).last)
//  println(graph.findShortestPathBFS())
//  graph.edges.foreach(println)
//  def maxFlow(graph: Graph, source: Node, destination: Node) = {
//
//  }

//  def findPath(graph: Graph, source: Node, destination: Node) = {
//    val visitedNodes =  Set[Node]()
//    val neighbors = graph.neighborhoods
//
//    val q: mutable.Queue[List[Node]] = new mutable.Queue
//  }

//  println(graph.findShortestPathBFS())
}