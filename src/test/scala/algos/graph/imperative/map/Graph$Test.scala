package algos.graph.imperative.map

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
 * Author: @aguestuser
 * Date: 4/9/15
 */





class Graph$Test extends Specification {

  trait SampleNodes extends Scope {

    lazy val (n0,n1,n2,n3) = (new Node(0), new Node(1), new Node(2), new Node(3))
    lazy val nodes = Map((0,n0),(1,n1),(2,n2),(3,n3))

  }

  "Graph object" should {

    "construct a graph" >> new SampleNodes {

      lazy val g = Graph(nodes)

      g.nodes == Map((0,n0),(1,n1),(2,n2),(3,n3))
      g.nodes(1) === n1
      g.nodes(2) === n2
      g.nodes(3) === n3

      g.nodes.get(666) === None
      g.nodes.values.map(_.adj) === List.fill(4)(Set())
    }

    "add a node" >> new SampleNodes {

      lazy val n4 = new Node(4)
      lazy val g = Graph.add(Graph(nodes), n4)
      val vs = g.nodes.values

      g.nodes === Map((0,n0),(1,n1),(2,n2),(3,n3),(4,n4))
      g.nodes(4) === n4

    }

    "add many nodes" >> new SampleNodes {

      lazy val (n4,n5) = (new Node(4), new Node(5))
      lazy val g = Graph.addMany(Graph(nodes), List(n4,n5))

      g.nodes === Map((0,n0),(1,n1),(2,n2),(3,n3),(4,n4),(5,n5))
      g.nodes(4) === n4
      g.nodes(5) === n5
    }

    "remove a node" >> new SampleNodes {

      lazy val g = Graph.remove(Graph(nodes), n0)

      g.nodes == Map((0,n0),(1,n1),(2,n2),(3,n3))
      g.nodes.get(0) === None
    }

    "remove many nodes" >> new SampleNodes {

      lazy val g = Graph.removeMany(Graph(nodes), List(n0,n1))

      g.nodes === Map((2,n2),(3,n3))
      g.nodes.get(0) === None
      g.nodes.get(1) === None

    }

    "connect an edge" >> new SampleNodes {

      lazy val g = Graph.connect(Graph(nodes), Edge(n0,n1))

      g.nodes(0).adj === Set(n1)
      g.nodes(1).adj === Set(n0)
    }

    "connect many edges" >> new SampleNodes {

      lazy val g = Graph.connectMany(Graph(nodes), List(Edge(n0,n1),Edge(n0,n2)))

      g.nodes(0).adj === Set(n1,n2)

    }

    "disconnect an edge" >> new SampleNodes {

      lazy val g1 = Graph.connectMany(Graph(nodes), List(Edge(n0,n1),Edge(n0,n2)))
      lazy val g2 = Graph.disconnect(g1, Edge(n0,n1))

      g2.nodes(0).adj === Set(n2)

    }

    "disconnect many edges" >> new SampleNodes {

      lazy val g1 = Graph.connectMany(Graph(nodes), List(Edge(n0,n1),Edge(n0,n2),Edge(n0,n3)))
      lazy val g2 = Graph.disconnectMany(g1, List(Edge(n0,n1),Edge(n0,n2)))

      g2.nodes(0).adj === Set(n3)

    }

  }

}
