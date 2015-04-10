package algos.graph.imperative.set

import org.specs2.mutable.{Before, Specification}
import org.specs2.specification.{Scope}


/**
 * Author: @aguestuser
 * Date: 4/9/15
 */

class BfSearch$Test extends Specification {

  trait SampleGraph extends Scope with Before {

    val ns = (0 to 7).toList map { new Node(_) }
    val g = Graph[Int](ns.toSet)
    
    def before = {
      ns(0).connectMany(List(ns(1),ns(2),ns(3)))
      ns(1).connectMany(List(ns(0),ns(2),ns(3)))
      ns(2).connectMany(List(ns(0),ns(1),ns(3)))
      ns(3).connectMany(List(ns(0),ns(1),ns(2),ns(5)))
      ns(4).connectMany(List(ns(5),ns(6),ns(7)))
      ns(5).connectMany(List(ns(3),ns(4),ns(6),ns(7)))
      ns(6).connectMany(List(ns(4),ns(5),ns(7)))
      ns(7).connectMany(List(ns(4),ns(5),ns(6)))
    }

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

  }

  "BfSearch object" should {

    "explore all nodes connected to a starting node" >> new SampleGraph {

      BfSearch.bfExplore(g,ns(0)) ===
        List(
          ns(1),
          ns(2),
          ns(3),
          ns(5),
          ns(4),
          ns(6),
          ns(7))

      BfSearch.bfExplore(g,ns(5)) ===
        List(
          ns(3),
          ns(4),
          ns(6),
          ns(7),
          ns(0),
          ns(1),
          ns(2))

    }

    "find the shortest path between two nodes" >> new SampleGraph {

      BfSearch.bfShortestPath(g,ns(2),ns(6)) === 3
      BfSearch.bfShortestPath(g,ns(3),ns(5)) === 1
      BfSearch.bfShortestPath(g,ns(0),ns(7)) === 3

    }

  }

}
