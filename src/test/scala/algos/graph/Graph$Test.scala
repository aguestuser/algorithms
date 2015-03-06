package algos.graph

import algos.graph.Graph._
import org.specs2.mutable.Specification

/**
 * Author: @aguestuser
 * Date: 3/6/15
 * License: GPLv2
 */

class Graph$Test extends Specification {

  lazy val vs = Vector(Vertex(1,List(2,3,4)),Vertex(2,List(1,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3)))
  lazy val g = Graph.construct(vs)

  "Graph object" should {

    "construct a graph" in {
      construct(vs) ===
        Graph(
          Vector(Vertex(1,List(2,3,4)),Vertex(2,List(1,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3))),
          Vector(Edge(1,2),Edge(1,3),Edge(1,4),Edge(2,3),Edge(2,4),Edge(3,4)))
    }

    "count the number of vertexes and edges in a graph" in {
      sizes(g) === (4,6)
    }

    "find a vertex if it exists" in {
      findv(g,2) === Some(Vertex(2,List(1,3,4)))
      findv(g,5) === None
    }

    "detect if a vertex exists" in {
      hasv(g,1) === true
      hasv(g,5) === false
    }

    "detect if an edge exists" in {
      hase(g,Edge(1,2)) === true
      hase(g,Edge(100,101)) === false
    }

    "add a vertex" in {
      val g2 = add(g,Vertex(5,List()))
      g2 === Graph(
        Vector(Vertex(1,List(2,3,4)),Vertex(2,List(1,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3)),Vertex(5,List())),
        Vector(Edge(1,2),Edge(1,3),Edge(1,4),Edge(2,3),Edge(2,4),Edge(3,4)))
      sizes(g2) === (5,6)
    }

    "connect two vertices" in {
      val g2 = add(g,Vertex(5,List()))
      val g3 = connect(g2,1,5)
      g3 === Graph(
        Vector(Vertex(1,List(5,2,3,4)),Vertex(2,List(1,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3)),Vertex(5,List(1))),
        Vector(Edge(1,2),Edge(1,3),Edge(1,4),Edge(2,3),Edge(2,4),Edge(3,4),Edge(1,5)))
      sizes(g3) === (5,7)
    }

    "contract a graph" in {
      val g2 = contract(g,0,1)
      g2 === Graph(
//        Vector(Vertex(1,List(2,3,4)),Vertex(2,List(1,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3))),
//        Vector(Edge(1,2),Edge(1,3),Edge(1,4),Edge(2,3),Edge(2,4),Edge(3,4)))
        Vector(Vertex(1,List(3,4,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3))),
        // actual: Vector(Edge(1,3), Edge(1,4), Edge(2,3), Edge(3,4), Edge(2,4)))
        Vector(Edge(1,3),Edge(1,3),Edge(1,4),Edge(1,4),Edge(2,3),Edge(2,4),Edge(3,4)))
      sizes(g2) === (3,7)
    }
  }

}
