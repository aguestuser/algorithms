package algos.graph

import algos.graph.Graph._
import org.specs2.mutable.Specification
import math.log

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
      findV(g,2) === Some(Vertex(2,List(1,3,4)))
      findV(g,5) === None
    }

    "detect if an edge exists" in {
      hasE(g,Edge(1,2)) === true
      hasE(g,Edge(100,101)) === false
    }

    "add a vertex" in {
      val g2 = add(g,Vertex(5,List()))
      g2 === Graph(
        Vector(Vertex(1,List(2,3,4)),Vertex(2,List(1,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3)),Vertex(5,List())),
        Vector(Edge(1,2),Edge(1,3),Edge(1,4),Edge(2,3),Edge(2,4),Edge(3,4)))
      sizes(g2) === (5,6)
    }

    "connect two vertices by index" in {

      val g2 = add(g,Vertex(5,List()))
      val g3 = connect(g2,0,4)
      g3 === Graph(
        Vector(Vertex(1,List(5,2,3,4)),Vertex(2,List(1,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3)),Vertex(5,List(1))),
        Vector(Edge(1,2),Edge(1,3),Edge(1,4),Edge(2,3),Edge(2,4),Edge(3,4),Edge(1,5)))
      sizes(g3) === (5,7)
    }

    "connect two vertices by key" in {
      val g2 = add(g,Vertex(5,List()))

      connectByKey(g2,1,1000) === g2
      connectByKey(g2,1000,1) === g2
      connectByKey(g2,1000,1000) === g2
      connectByKey(g2,1,5) === Graph(
        Vector(Vertex(1,List(5,2,3,4)),Vertex(2,List(1,3,4)),Vertex(3,List(1,2,4)),Vertex(4,List(1,2,3)),Vertex(5,List(1))),
        Vector(Edge(1,2),Edge(1,3),Edge(1,4),Edge(2,3),Edge(2,4),Edge(3,4),Edge(1,5)))
    }

    "contract a vector of vertexes" in {
      contractVs(g.vs,0,1) === Vector(Vertex(1,List(3,4,3,4)), Vertex(3,List(1,1,4)), Vertex(4,List(1,1,3)))
    }

    "contract a vector of edges" in {
      contractEs(g.es,g.vs,0,1) === Vector(Edge(1,3),Edge(1,4),Edge(1,3),Edge(1,4),Edge(3,4))
    }

    "contract a graph" in {
      val g2 = contract(g,0,1)
      g2 === Graph(
        Vector(Vertex(1,List(3,4,3,4)), Vertex(3,List(1,1,4)), Vertex(4,List(1,1,3))),
        Vector(Edge(1,3),Edge(1,4),Edge(1,3),Edge(1,4),Edge(3,4)))
      sizes(g2) === (3,5)
    }

    "recursively and randomly contract a graph" in {
      sizes(randomContract(g))._1 === 2
    }

    "parse a graph from text input" in {
      parseTxt("src/test/resources/sampleGraph.txt") ===
        construct(Vector(
          Vertex(1,List(2,3,4)),
          Vertex(2,List(1,3,4)),
          Vertex(3,List(1,2,4)),
          Vertex(4,List(1,2,3))))
    }

    "tell me what i need to know" in {
      parseTxt("src/test/resources/minCutSampleData.txt").es.size === 2517
    }

    "compute the min cut of a small graph " in {
      rContractN(g,16) === 3
    }

    "compute the min cut of a large graph" in {
      val g1 = parseTxt("src/test/resources/minCutSampleData.txt")
      val n = sizes(g1)._1
      rContractN(g1,n * n * log(n).toInt) === 21
    }
  }

}
