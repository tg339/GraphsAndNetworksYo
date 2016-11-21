import java.io.File

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}

/**
  * Created by timdelisle on 10/23/16.
  */

/**
  * Node ID reference to fig 7.3
  * --------
  * PA: 0 {4,12,5}
  * PB: 1 {7,10,9}
  * PC: 2 {7,7,10}
  *
  * H1: 3 price: 5
  * H2: 4 price: 8
  * H3: 5 price: 6
  */
object GraphFactories {
  def testGraph1 = {
    val nodes = Seq(Node(1),Node(2),Node(3),Node(4),Node(5),Node(6))

    val buyers: Map[Node, Map[Node, Float]] = Map(
      nodes(0) -> Map(nodes(3) -> 4, nodes(4) -> 12, nodes(5) -> 5),
      nodes(1) -> Map(nodes(3) -> 7, nodes(4) -> 10, nodes(5) -> 9),
      nodes(2) -> Map(nodes(3) -> 7, nodes(4) -> 7, nodes(5) -> 10)
    )
    val items: Map[Node, Float] = Map(
      nodes(3) -> 0,
      nodes(4) -> 3,
      nodes(5) -> 2
    )

    val nodeSplits = nodes.splitAt(3)
    val pairs = for (x <- nodeSplits._1; y <- nodeSplits._2) yield (x,y)
    val edges = pairs.map(e => (e._1.id, e._2.id) -> Edge(e._1, e._2, 0,0)).toMap

    new Matching(buyers, items, edges)
  }

  def testGraph2 = {
    val nodes = Seq(Node(1),Node(2),Node(3),Node(4),Node(5),Node(6))

    val buyers: Map[Node, Map[Node, Float]] = Map(
      nodes(0) -> Map(nodes(3) -> 10, nodes(4) -> 0, nodes(5) -> 0),
      nodes(1) -> Map(nodes(3) -> 0, nodes(4) -> 12, nodes(5) -> 0),
      nodes(2) -> Map(nodes(3) -> 0, nodes(4) -> 12, nodes(5) -> 0)
    )
    val items: Map[Node, Float] = Map(
      nodes(3) -> 10,
      nodes(4) -> 12,
      nodes(5) -> 1000
    )

    val nodeSplits = nodes.splitAt(3)
    val pairs = for (x <- nodeSplits._1; y <- nodeSplits._2) yield (x,y)
    val edges = pairs.map(e => (e._1.id, e._2.id) -> Edge(e._1, e._2, 0,0)).toMap

    new Matching(buyers, items, edges)
  }

  def maxFlowTestGraph = {
    val nodes = Seq(Node(0), Node(1), Node(2), Node(3))
    val edges: Map[(Int,Int), Edge] = Map (
      (0,1) -> Edge(nodes(0),nodes(1),1,0.0),
      (1,3) -> Edge(nodes(1),nodes(3),1,0.0),
      (0,2) -> Edge(nodes(0),nodes(2),3,0.0),
      (1,2) -> Edge(nodes(1),nodes(2),2,0.0),
      (2,3) -> Edge(nodes(2),nodes(3),1,0.0)
    )
    new Graph(nodes.toSet, edges)
  }

  def secondTestGraph = {
    val nodes = Seq(Node(0), Node(1), Node(2), Node(3),Node(4))
    val edges: Map[(Int,Int), Edge] = Map(
      (0,1) -> Edge(Node(0),Node(1),1,1.0),
      (0,2) -> Edge(Node(0),Node(2),2,1.0),
      (0,3) -> Edge(Node(0),Node(3),3,1.0),
      (1,4) -> Edge(Node(1),Node(4),1,1.0),
      (1,2) -> Edge(Node(1),Node(2),2,1.0),
      (2,4) -> Edge(Node(2),Node(4),2,1.0),
      (2,3) -> Edge(Node(2),Node(3),1,1.0),
      (3,4) -> Edge(Node(3),Node(4),1,1.0)
    )
    new Graph(nodes.toSet, edges)
  }


  def simpleTestGraphs = {
    val nodes = Set((0 to 3).map(Node): _*)
    val edges = Map(
      (0,2) -> Edge(Node(0),Node(2),1,1.0),
      (2,0) -> Edge(Node(2),Node(0),1,1.0),
      (2,3) -> Edge(Node(2),Node(3),1,1.0),
      (3,2) -> Edge(Node(3),Node(2),1,1.0),
      (0,1) -> Edge(Node(0),Node(1),1,1.0),
      (1,0) -> Edge(Node(1),Node(0),1,1.0),
      (3,1) -> Edge(Node(3),Node(1),1,1.0),
      (1,3) -> Edge(Node(1),Node(3),1,1.0)
    )

    List(
      new Graph(nodes, edges),
      new Graph(nodes + Node(4), edges + ((1,4) -> Edge(Node(1),Node(4),1,1.0)) + ((4,1) -> Edge(Node(4),Node(1),1,1.0)))
    )

  }

  def fromFile(path: String) = {
    implicit object MyFormat extends DefaultCSVFormat {
      override val delimiter = ' '
    }

    val reader = CSVReader.open(new File(path))
    val lines = reader.all()
    reader.close()
    val directEdges = lines.flatMap(x => List(Edge(Node(x.head.toInt), Node(x(1).toInt), 1,1), Edge(Node(x(1).toInt), Node(x.head.toInt), 1,1)))
//    val unDirectEdges = lines.map(x => Edge(Node(x.head.toInt), Node(x(1).toInt), 1,1))
    val nodes = lines.flatMap(x => List(Node(x.head.toInt), Node(x(1).toInt)))
    new Graph(edges = directEdges.map(x => (x.to.id, x.from.id) -> x).toMap, vertices = nodes.toSet)
  }

  def fromNumberAndProb(n: Int, p: Float) = {
    /**
      * n: Number of nodes in the graph
      * p: probability of an edge between two notes
      */
    val r = scala.util.Random
    val singleNodes = (0 until n).map(x => Node(x)).toSet
    val combos = for (x <- singleNodes; y <- singleNodes if r.nextFloat <= p) yield Edge(x, y, 1, 1)

    new Graph(singleNodes, combos.map(x => (x.to.id, x.from.id) -> x).toMap)
  }
}
