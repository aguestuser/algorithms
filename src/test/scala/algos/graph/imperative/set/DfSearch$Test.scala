package algos.graph.imperative.set

import org.specs2.mutable.{Before, Specification}
import org.specs2.specification.Scope

/**
 * Author: @aguestuser
 * Date: 4/10/15
 */

class DfSearch$Test extends Specification {

  "DfSearch" should {

    trait SampleGraph extends Scope with Before {

      val ns = (0 to 7).toList map { new Node(_) }
      val g = Graph[Int](ns.toSet)

      def before = {
        ns(0).connectMany(List(ns(1),ns(2),ns(3)))
        ns(1).connectMany(List(ns(3),ns(0),ns(2)))
        ns(2).connectMany(List(ns(0),ns(1),ns(3)))
        ns(3).connectMany(List(ns(5),ns(0),ns(1),ns(2)))
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

    "explore every explorable node in depth-first order" >> new SampleGraph {

      DfSearch.explore(g,ns(0)) ===
        List(
          ns(0),
          ns(1),
          ns(3),
          ns(5),
          ns(4),
          ns(6),
          ns(7),
          ns(2))

      DfSearch.explore(g,ns(5)) ===
        List(
          ns(5),
          ns(3),
          ns(0),
          ns(1),
          ns(2),
          ns(4),
          ns(6),
          ns(7))

    }

    trait SampleDirectedGraph extends Scope with Before {

      val ns = (0 to 7).toList map { new Node(_) }
      val g = Graph[Int](ns.toSet)

      def before = {
        ns(0).connectMany(List(ns(1),ns(2)))
        ns(1).connectMany(List(ns(3)))
        ns(2).connectMany(List(ns(3)))
        ns(3).connectMany(List(ns(5)))
        ns(4).connectMany(List(ns(6)))
        ns(5).connectMany(List(ns(4),ns(7)))
        ns(7).connectMany(List(ns(6)))
      }

      /*
      *
      *  0 --> 1
      *  |     |
      *  |     |
      *  v     v
      *  2 --> 3
      *        |
      *        v
      *  4 <-- 5
      *  |     |
      *  |     |
      *  v     v
      *  6 <-- 7
      *
      * */

    }

    "sort a graph into topological order" >> new SampleDirectedGraph {

      val map = DfSearch.topoSort(g)
        ns(0) === map(1)
        Set(ns(1),ns(2)) must contain(map(2))
        Set(ns(1),ns(2)) must contain(map(3))
        ns(3) === map(4)
        ns(5) === map(5)
        Set(ns(4),ns(7)) must contain(map(6))
        Set(ns(4),ns(7)) must contain(map(7))
        ns(6) === map(8)


    }

  }

}

