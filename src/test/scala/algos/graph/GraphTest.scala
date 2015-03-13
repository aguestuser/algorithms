package algos.graph

import org.specs2.mutable.Specification

/**
 * Author: @aguestuser
 * Date: 3/12/15
 * License: GPLv2
 */


class GraphTest extends Specification {

  "Graph class" should {

    "add nodes" in {

      lazy val g = new Graph[Int].addNode(1).addNode(2).addNode(3)
      g.ns.size === 3
//      g.es.size === 0

      g.ns(0).value === 1
      g.ns(0).adj.size === 0

      g.ns(1).value === 2
      g.ns(1).adj.size === 0

      g.ns(2).value === 3
      g.ns(2).adj.size === 0
    }

    "connect nodes" in {

      lazy val g = new Graph[Int]
        .addNode(1).addNode(2).addNode(3).addNode(4)
        .connect(0,1).connect(0,2).connect(0,3).connect(1,2)

      g.ns.size === 4
//      g.es.size === 4

      g.ns(0).adj.size === 3
      g.ns(1).adj.size === 2
      g.ns(2).adj.size === 2
      g.ns(3).adj.size === 1

    }

//    "disconnect nodes" in {
//      lazy val g = new Graph[Int].addNode(1).addNode(2).connect(0,1).disconnect(1,0)
//      g.es.size === 0
//    }
//
//    "not double count edges" in {
//      lazy val g = new Graph[Int].addNode(1).addNode(2).connect(0,1).connect(1,0)
//      g.es.size === 1
//    }
//
//    "add double edges" in {
//      lazy val g = new Graph[Int].addNode(1).addNode(2).connect(0,1).connect(0,1)
//      g.es.size === 2
//    }

    "contract a graph" in {


      lazy val g = new Graph[Int]
        .addNodes(List(0,1,2,3,4,5,6,7))
        .connectMany(
          List(
            (0,1),(0,2),(0,3),
            (1,2),(1,3),
            (2,3),
            (3,5),
            (4,5),(4,6),(4,7),
            (5,6),(5,7),
            (6,7)))

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


      "base case " in {
        g.ns.size === 8
//        g.es.size === 13
      }

      "with two connected edges" in {
        lazy val g2 = g.contract(0,1)
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

        g2.ns.size === 7
//        g2.es.size === 12

      }

      "with two un-connected edges" in {
        lazy val g3 = g.contract(2,4)

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
        g3.ns.size === 7
//        g3.es.size === 13
      }

      "with two contractions in a row" in {
        lazy val g3 = g.contract(0,1).contract(0,1)
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

        g3.ns.size === 6
//        g3.es.size === 10
       }
    }

    "find the min cut" in {
      lazy val g = new Graph[Int]
        .addNodes(List(0,1,2,3,4,5,6,7))
        .connectMany(
          List(
            (0,1),(0,2),(0,3),
            (1,2),(1,3),
            (2,3),
            (3,5),
            (4,5),(4,6),(4,7),
            (5,6),(5,7),
            (6,7)))
      val mc = g.minCut
      println(s"value: ${g.ns.head.value} \n adjs: ${g.ns.head.adj.map(_.value)}")
      mc === 1
    }

//    "parse a graph from a txt adj list" in {
//
//      val g1 = new Graph[Int]
//        .addNodes(List(1,2,3,4))
//        .connectMany(List((0,1),(0,2),(0,3),(1,2),(1,3),(2,3)))
//      val g2 = Graph.parseTxt("src/test/resources/sampleGraph.txt")
//
//      g1.ns.size === g2.ns.size
////      g1.es.size === g2.es.size
//      g1.ns.map(_.value) === g1.ns.map(_.value)
////      g1.es.map({case(ni,nj) => (ni.value,nj.value) }) === g2.es.map({case(ni,nj) => (ni.value,nj.value) })
//
//    }

    "find the min cut of a large graph" in {
      val g = Graph.parseTxt("src/test/resources/minCutSampleData.txt")
      g.minCut === 20
    }
  }
}
