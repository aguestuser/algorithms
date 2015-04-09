package algos.graph.imperative.set

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
 * Author: @aguestuser
 * Date: 4/9/15
 */

class Graph$Test extends Specification {

  trait SampleNodes extends Scope {

    lazy val (n0,n1,n2,n3) = (new Node(0), new Node(1), new Node(2), new Node(3))
    lazy val nodes = Set(n0,n1,n2,n3)

  }

  "Graph object" should {

    "construct a graph" >> new SampleNodes {

      lazy val g = Graph(nodes)

      g.nodes === nodes

      g.nodes.contains(n0) === true
      g.nodes.contains(n1) === true
      g.nodes.contains(n2) === true
      g.nodes.contains(n3) === true
      g.nodes.contains(new Node(666)) === false

      g.nodes.map(_.adj) === Set(Set(),Set(),Set(),Set())
    }

    "add a node" >> new SampleNodes {

      lazy val n4 = new Node(4)
      lazy val g = Graph.add(Graph(nodes), n4)

      g.nodes === Set(n0,n1,n2,n3,n4)

    }

    "add many nodes" >> new SampleNodes {

      lazy val (n4,n5) = (new Node(5), new Node(6))
      lazy val g = Graph.addMany(Graph(nodes), List(n4,n5))

      g.nodes === Set(n0,n1,n2,n3,n4,n5)
    }

    "remove a node" >> new SampleNodes {

      lazy val g = Graph.remove(Graph(nodes), n0)

      g.nodes === Set(n1,n2,n3)
    }

    "remove many nodes" >> new SampleNodes {

      lazy val g = Graph.removeMany(Graph(nodes), List(n0,n1))

      g.nodes === Set(n2,n3)
    }

    "not remove a node that doesn't exist" >> new SampleNodes {

      lazy val g1 = Graph(nodes)
      lazy val g2 = Graph.remove(g1, new Node(666))

      g1.nodes === g2.nodes
    }

    "connect an edge" >> new SampleNodes {

      val g = Graph.connect(Graph(nodes), Edge(n0,n1))

      n0.adj === Set(n1)
      n1.adj === Set(n0)
    }

    "connect many edges" >> new SampleNodes {

      val g = Graph.connectMany(Graph(nodes), List(Edge(n0,n1),Edge(n0,n2)))

      n0.adj === Set(n1,n2)
    }

    "not connect a node to a node that isn't in the graph" >> new SampleNodes {

      val fakeNode = new Node(666)
      val g1 = Graph.connect(Graph(nodes),Edge(n0,n1))
      val g2 = Graph.connect(g1, Edge(n0,fakeNode))

      n0.adj === Set(n1)
      fakeNode.adj === Set()
    }

    "disconnect an edge" >> new SampleNodes {

      val g1 = Graph.connectMany(Graph(nodes), List(Edge(n0,n1),Edge(n0,n2)))
      val g2 = Graph.disconnect(g1, Edge(n0,n1))

      n0.adj === Set(n2)
    }

    "disconnect many edges" >> new SampleNodes {

      val g1 = Graph.connectMany(Graph(nodes), List(Edge(n0,n1),Edge(n0,n2),Edge(n0,n3)))
      val g2 = Graph.disconnectMany(g1, List(Edge(n0,n1),Edge(n0,n2)))

      n0.adj === Set(n3)
    }

    "not disconnect two nodes that aren't connected" >> new SampleNodes {

      val g = Graph.disconnect(Graph(nodes),Edge(n0,n1))

      Graph.contains(g,List(n0,n1)) === true
      n0.adj === Set()
      n1.adj === Set()
    }

    "not disconnect a node from a node that isn't in the graph" >> new SampleNodes {

      val fakeNode = new Node(666)
      val g = Graph.disconnect(Graph(nodes),Edge(n0,fakeNode))

      Graph.contains(g,List(fakeNode)) === false
      Graph.contains(g,List(n0)) === true
      n0.adj === Set()

    }

  }

}