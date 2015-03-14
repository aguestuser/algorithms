package algos.graph

import Graph._

import org.specs2.mutable.Specification

/**
 * Author: @aguestuser
 * Date: 3/12/15
 * License: GPLv2
 */


class Graph$Test extends Specification {

  "Graph object" should {

    "construct a graph" in {

      lazy val g = Graph(
        Vector(
          Node(0,List(1,2,3)),
          Node(1,List(0,2)),
          Node(2,List(0,1,3)),
          Node(3,List(0,2))))

      g.ns.size === 4
      g.ns(0).adj.size === 3
      g.ns(1).adj.size === 2
      g.ns(2).adj.size === 3
      g.ns(1).adj.size === 2
    }

    "connect two nodes" in {
      lazy val g = Graph(Vector(Node(0,Nil),Node(1,Nil),Node(2,Nil),Node(3,Nil)))
      Graph.connect(g,0,1) ===
        Graph(Vector(Node(0,List(1)),Node(1,List(0)),Node(2,Nil),Node(3,Nil)))
    }

    "connect many nodes" in {
      lazy val g = Graph(Vector(Node(0,Nil),Node(1,Nil),Node(2,Nil),Node(3,Nil)))
      Graph.connectMany(g,List((0,1),(0,2),(2,3))) ===
        Graph(Vector(Node(0,List(2,1)),Node(1,List(0)),Node(2,List(3,0)),Node(3,List(2))))
    }

    "contract a graph" in {

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

      "with two connected edges" in {
        Graph.contract(g,0,1) === Graph(
          Vector(
            Node(0,List(2,3,2,3)),
            Node(2,List(0,0,3)),
            Node(3,List(0,0,2,5)),
            Node(4,List(5,6,7)),
            Node(5,List(3,4,6,7)),
            Node(6,List(4,5,7)),
            Node(7,List(4,5,6))))
        /*
        *
        *     0
        *   // \\
        *  //  \\
        *  2 -- 3
        *       |
        *  4 -- 5
        *  | \/ |
        *  | /\ |
        *  6 -- 7
        *
        * */
      }

      "with two un-connected edges" in {
        Graph.contract(g,2,4) === Graph(
          Vector(
            Node(0,List(1,2,3)),
            Node(1,List(0,2,3)),
            Node(2,List(0,1,3,5,6,7)),
            Node(3,List(0,1,2,5)),
            Node(5,List(3,2,6,7)),
            Node(6,List(2,5,7)),
            Node(7,List(2,5,6))))
        /*
        *
        *       0 -- 1
        *       | \/ |
        *       | /\ |
        *       2 -- 3
        *     / | \  |
        *    /  |  \ |
        *   6 - 7 -- 5
        *    \______/
        *
        * */
      }

      "with 2 contractions in a row" in {
        contract(contract(g,0,1),0,1) === Graph(
          Vector(
            Node(0,List(3,3,3)),
            Node(3,List(0,0,0,5)),
            Node(4,List(5,6,7)),
            Node(5,List(3,4,6,7)),
            Node(6,List(4,5,7)),
            Node(7,List(4,5,6))))
        /*
        *
        *       0
        *      |||
        *      |||
        *       3
        *       |
        *  4 -- 5
        *  | \/ |
        *  | /\ |
        *  6 -- 7
        *
        * */
      }

      "with 3 contractions in a row" in {
        contract(contract(contract(g,0,1),0,1),1,3) === Graph(
          Vector(
            Node(0,List(3,3,3)),
            Node(3,List(0,0,0,4,6,7)),
            Node(4,List(3,6,7)),
            Node(6,List(4,3,7)),
            Node(7,List(4,3,6))))
        /*
        *
        *       0
        *      |||
        *      |||
        *       3
        *    /  |  \
        *   4 - 6 - 7
        *    \_____/
        *
        * */

      }

      "with 4 contractions in a row" in {
        contract(contract(contract(contract(g,0,1),0,1),1,3),0,1) === Graph(
          Vector(
            Node(0,List(4,6,7)),
            Node(4,List(0,6,7)),
            Node(6,List(4,0,7)),
            Node(7,List(4,0,6))))
        /*
        *
        *       0
        *    /  |  \
        *   4 - 6 - 7
        *    \_____/
        *
        * */
      }

      "with 4 different contractions" in {
        contract(contract(contract(contract(g,0,1),0,1),1,3),1,0) === Graph(
          Vector(
            Node(3,List(4,6,7)),
            Node(4,List(3,6,7)),
            Node(6,List(4,3,7)),
            Node(7,List(4,3,6))))
        /*
        *
        *       3
        *    /  |  \
        *   4 - 6 - 7
        *    \_____/
        *
        * */
      }

      "with the same indices" in {
        contract(g,0,0) === g
      }

      "to find the min cut" in {
        Graph.minCut(g) === 1
      }
     }

    "parse a graph from text input" in {
      parseTxt("src/test/resources/sampleGraph.txt") ===
        Graph(
          Vector(
            Node(1,List(2,3,4)),
            Node(2,List(1,3,4)),
            Node(3,List(1,2,4)),
            Node(4,List(1,2,3))))
    }

    "find the min cut of a small graph" in {
      minCut(parseTxt("src/test/resources/minCutTest0.txt")) === 1
    }

    "find the min cut of a medium graph" in {
      minCut(parseTxt("src/test/resources/minCutTest1.txt")) === 3
    }

    "find the min cut of a large graph" in {
      minCut(parseTxt("src/test/resources/minCutSampleData.txt")) === 17
    }
  }
}
