import java.io.File

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}

import util.Random
import collection.mutable
import util.control.Breaks._

//case class Edge(to: Node, from: Node, weight: Option[Int]=Some(1))
//case class Node(identifier: String, var visited: Boolean=false)

//object GraphFactories {
//  def fromFile(path: String) = {
//    implicit object MyFormat extends DefaultCSVFormat {
//      override val delimiter = ' '
//    }
//
//    val reader = CSVReader.open(new File(path))
//    val lines = reader.all()
//    reader.close()
////        val directEdges = lines.flatMap(x => List(Edge(Node(x.head), Node(x(1))), Edge(Node(x(1)), Node(x.head))))
//    val unDirectEdges = lines.map(x => Edge(Node(x.head), Node(x(1))))
//    val nodes = lines.flatMap(x => List(Node(x.head), Node(x(1))))
//    new Graph(edges = unDirectEdges.toSet, nodes.toSet)
//  }
//
//  def fromNumberAndProb(n: Int, p: Float) = {
//    /**
//      * n: Number of nodes in the graph
//      * p: probability of an edge between two notes
//      */
//    val r = scala.util.Random
//    val singleNodes = (0 until n).map(x => Node(x.toString)).toSet
//    val combos = for (x <- singleNodes; y <- singleNodes if r.nextFloat <= p) yield Edge(x, y)
//
//    new Graph(combos, singleNodes)
//  }
//
//  def runTest(g: Graph, maxTimes: Int) = {
//    val r = scala.util.Random
//
//    val timesToRun = (1 to maxTimes).toList.par
//
//    val results = timesToRun.map(x => {
//      val startIdx = r.nextInt(g.nodes.toVector.length)
//      val endIdx = r.nextInt(g.nodes.toVector.length)
//
//      val pathLength = g.findShortestPathBFS(g.nodes.toVector(startIdx), g.nodes.toVector(endIdx)) match {
//        case Nil => 20
//        case x: List[String] => x.length
//      }
//      (g.nodes.toVector(startIdx), g.nodes.toVector(endIdx), pathLength)
//    })
//    results.map(_._3).sum.toDouble/results.length.toDouble
//  }
//}

////class Graph(val edges: Set[Edge], vertices: Set[Node]) {
////  val nodes: Set[Node] = vertices ++ edges.map(_.to) ++ edges.map(_.from)
////
////  def addEdge(e: Edge) = new Graph(edges + e, nodes + e.to + e.from)
////  def addVertex(v: Node) = new Graph(edges,nodes + v)
////
////  var neighborhoods: Map[Node, Set[Node]] = edges.groupBy(_.from).mapValues(x => x.map(_.to))
////
////
////
////
////  def findShortestPathBFS(startNode: Node= Random.shuffle(nodes.toList).head, endNode: Node = Random.shuffle(nodes.toList).last): List[String] = {
////    val parents: mutable.Map[String, String] = mutable.Map.empty
////    val q: mutable.Queue[Node] = new mutable.Queue
////    var path: List[String] = List()
////    q.enqueue(startNode)
////    breakable {
////      while(q.nonEmpty) {
////        val node = q.dequeue()
////        if(node == endNode) {
////          path = backTrace(parents, startNode, endNode)
////          break
////        }
////        if(!node.visited) {
////          neighborhoods.getOrElse(node, mutable.Set.empty).filter(n => !n.visited && n.identifier != node.identifier).foreach(neighbor =>{
////            if(neighbor.identifier == endNode.identifier) {
////              parents(neighbor.identifier) = node.identifier
////              path = backTrace(parents, startNode, endNode)
////              break
////            } else {
////              parents(neighbor.identifier) = node.identifier
////              q.enqueue(neighbor)
////            }
////          })
////        }
////        node.visited = true
////      }
////
////    }
////    path
////  }
//}