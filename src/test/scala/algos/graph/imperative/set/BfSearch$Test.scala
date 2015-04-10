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

      BfSearch.explore(g,ns(0)) ===
        List(
          ns(0),
          ns(1),
          ns(2),
          ns(3),
          ns(5),
          ns(4),
          ns(6),
          ns(7))

      BfSearch.explore(g,ns(5)) ===
        List(
          ns(5),
          ns(3),
          ns(4),
          ns(6),
          ns(7),
          ns(0),
          ns(1),
          ns(2))

    }

    "find the shortest path between two nodes" >> new SampleGraph {

      BfSearch.shortestPath(g,ns(2),ns(6)) === 3
      BfSearch.shortestPath(g,ns(3),ns(5)) === 1
      BfSearch.shortestPath(g,ns(0),ns(7)) === 3
      BfSearch.shortestPath(g,ns(1),ns(1)) === 0

    }

    "find the connected components in a graph" >> new SampleGraph {

      val g2 = Graph.disconnect(g,Edge(ns(3),ns(5)))

      BfSearch.connectedComponents(g2) ===
        Set(
          Set(ns(4), ns(5), ns(6), ns(7)), // WRONG! (should have ns(6)
          Set(ns(1), ns(2), ns(3), ns(0)))

    }

    "generate a map of hops from a start point" >> new SampleGraph {

      BfSearch.mapHops(g,ns(0)) ===
        Map(
          0 → Set(ns(0)),
          1 → Set(ns(1),ns(2),ns(3)),
          2 → Set(ns(5)),
          3 → Set(ns(4),ns(6),ns(7)))

    }

  }

}
