import java.io.File

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}

/**
  * Created by timdelisle on 10/23/16.
  */
object GraphFactories {
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
    new Graph(edges = directEdges.toSet, vertices = nodes.toSet)
  }

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
}
