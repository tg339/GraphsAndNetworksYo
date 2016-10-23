import util.Random
import collection.mutable
import util.control.Breaks._

object GraphOps {
  def backTrace(parents: mutable.Map[Int, Int], start: Node, end: Node) = {
    println("start: " + start.id.toString)
    println("end: " + end.id.toString)
    println("parents: " + parents)

    var path = List(end.id)
    while(path.head != start.id){
      println(path.head)
      path = parents(path.head)::path
    }
    path
  }


  def beadthFirstSearch(graph: Graph, startNode: Node, targetNode: Node): List[Int] = {
    val parents: mutable.Map[Int, Int] = mutable.Map.empty
    val q: mutable.Queue[Node] = new mutable.Queue
    var path: List[Int] = List()

    q.enqueue(startNode)
    if(startNode == targetNode) path = List(startNode.id, targetNode.id)
    else {
      breakable {
        while (q.nonEmpty) {
          val node = q.dequeue()
          node.visited = true
          graph.neighborhoods(node.id).foreach(neighbor => {
            if(!neighbor.visited) {
              if(!parents.get(node.id).contains(neighbor.id)) parents(neighbor.id) = node.id
              neighbor.visited = true
              if(neighbor.id == targetNode.id) {
                path = backTrace(parents, startNode, targetNode)
                break
              }
              q.enqueue(neighbor)
            }
          })
        }
      }

    }
    path
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