package algos.graph.imperative.vector

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
 * Author: @aguestuser
 * Date: 4/7/15
 */


class Graph$Test extends Specification {

  "Graph object" should {

    "construct a graph" >> new SampleNodes {

      lazy val g = Graph(nodes)

      g.nodes.size === 4
      (g.nodes map { n:Node[Int] => n.adj }) === Vector.fill(4)(Set())

    }

    "add a node" >> new SampleNodes {

      lazy val g = Graph.add(Graph(nodes), new Node(4))

      g.nodes.size === 5
      g.nodes(4).item === 4

    }

    "remove a node" >> new SampleNodes {

      val g = Graph.remove(Graph(nodes), n0)

      g.nodes.size === 3
      g.nodes.head === n1

    }

    "connect an edge" >> new SampleNodes {

      lazy val g = Graph.connect(Graph(nodes),Edge(n0,n1))

      g.nodes.head.adj === Set(n1)
      g.nodes(1).adj === Set(n0)

    }

    "connect an edge by index" >> new SampleNodes {

      lazy val g = Graph.connectByIndex(Graph(nodes), 0, 1)

      g.nodes.head.adj === Set(n1)
      g.nodes(1).adj === Set(n0)

    }

    "connect many edges" >> new SampleNodes {

      lazy val g = Graph.connectMany(
        Graph(nodes),
        List(
          Edge(n0,n1), Edge(n0,n2), Edge(n0,n3),
          Edge(n1,n2), Edge(n1,n3),
          Edge(n2,n3)))

      g.nodes.head.adj === Set(n1,n2,n3)
      g.nodes(1).adj === Set(n0,n2,n3)
      g.nodes(2).adj === Set(n0,n1,n3)

    }

    "connect many edges by index" >> new SampleNodes {

      val g = Graph.connectManyByIndex(
        Graph(nodes),
        List(
          (0,1), (0,2),(0,3),
          (1,2),(1,3),
          (2,3)))

      g.nodes.head.adj === Set(n1,n2,n3)
      g.nodes(1).adj === Set(n0,n2,n3)
      g.nodes(2).adj === Set(n0,n1,n3)
      g.nodes(3).adj === Set(n0,n1,n2)

    }

    "disconnect an edge" >> new SampleNodes {

      val g1 = Graph.connectMany(Graph(nodes), List(Edge(n0,n1),Edge(n0,n2),Edge(n1,n2)))
      val g2 = Graph.disconnect(Graph(nodes),Edge(n0,n1))

      g2.nodes.head.adj === Set(n2)
      g2.nodes(1).adj === Set(n2)

    }
  }
}

trait SampleNodes extends Scope {

  lazy val (n0,n1,n2,n3) = (new Node(0), new Node(1), new Node(2), new Node(3))
  lazy val nodes = Vector(n0,n1,n2,n3)
}

