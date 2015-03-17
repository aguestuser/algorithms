package algos.graph

import algos.graph.BfSearch._
import org.specs2.mutable.Specification

/**
 * Author: aguestuser
 * Date: 3/17/15
 * License: GPLv3
 */

class BfSearch$Test extends Specification {

  "Breadth First Search" should {

    lazy val g = Graph(
      Vector(
        Node(0,List(1,2,3)),
        Node(1,List(0,2,3)),
        Node(2,List(0,1,3)),
        Node(3,List(0,1,2,5)),
        Node(4,List(5,6,7)),
        Node(5,List(3,4,6,7)),
        Node(6,List(4,5,7)),
        Node(7,List(4,5,6))))
    /*
    *
    *  0 -- 1
    *  | \/ |
    *  | /\ |
    *  2 -- 3
    *       |
    *  4 -- 5
    *  | \/ |
    *  | /\ |
    *  6 -- 7
    *
    * */

    "find every node in the path of a starting node" in {
      bfExplore(g,5) ===
        List(
          Node(3,List(0, 1, 2, 5)),
          Node(4,List(5, 6, 7)),
          Node(6,List(4, 5, 7)),
          Node(7,List(4, 5, 6)),
          Node(0,List(1, 2, 3)),
          Node(1,List(0, 2, 3)),
          Node(2,List(0, 1, 3)))
    }

    "find the shortest path between two nodes" in {
      bfShortestPath(g,2,6) === 3
      bfShortestPath(g,3,5) === 1
      bfShortestPath(g,0,7) === 3

    }

  }
}
