package algos.graph

import algos.graph.OldGraph._
import org.specs2.mutable.Specification
import math.log

/**
 * Author: @aguestuser
 * Date: 3/6/15
 * License: GPLv2
 */

class OldGraph$Test extends Specification {

  lazy val vs = Vector(OldVertex(1,List(2,3,4)),OldVertex(2,List(1,3,4)),OldVertex(3,List(1,2,4)),OldVertex(4,List(1,2,3)))
  lazy val g = OldGraph.construct(vs)

  "Graph object" should {

    "construct a graph" in {
      construct(vs) ===
        OldGraph(
          Vector(OldVertex(1,List(2,3,4)),OldVertex(2,List(1,3,4)),OldVertex(3,List(1,2,4)),OldVertex(4,List(1,2,3))),
          Vector(OldEdge(1,2),OldEdge(1,3),OldEdge(1,4),OldEdge(2,3),OldEdge(2,4),OldEdge(3,4)))
    }

    "count the number of vertexes and edges in a graph" in {
      sizes(g) === (4,6)
    }

    "find a vertex if it exists" in {
      findV(g,2) === Some(OldVertex(2,List(1,3,4)))
      findV(g,5) === None
    }

    "detect if an edge exists" in {
      hasE(g,OldEdge(1,2)) === true
      hasE(g,OldEdge(100,101)) === false
    }

    "add a vertex" in {
      val g2 = add(g,OldVertex(5,List()))
      g2 === OldGraph(
        Vector(OldVertex(1,List(2,3,4)),OldVertex(2,List(1,3,4)),OldVertex(3,List(1,2,4)),OldVertex(4,List(1,2,3)),OldVertex(5,List())),
        Vector(OldEdge(1,2),OldEdge(1,3),OldEdge(1,4),OldEdge(2,3),OldEdge(2,4),OldEdge(3,4)))
      sizes(g2) === (5,6)
    }

    "connect two vertices by index" in {

      val g2 = add(g,OldVertex(5,List()))
      val g3 = connect(g2,0,4)
      g3 === OldGraph(
        Vector(OldVertex(1,List(5,2,3,4)),OldVertex(2,List(1,3,4)),OldVertex(3,List(1,2,4)),OldVertex(4,List(1,2,3)),OldVertex(5,List(1))),
        Vector(OldEdge(1,2),OldEdge(1,3),OldEdge(1,4),OldEdge(2,3),OldEdge(2,4),OldEdge(3,4),OldEdge(1,5)))
      sizes(g3) === (5,7)
    }

    "connect two vertices by key" in {
      val g2 = add(g,OldVertex(5,List()))

      connectByKey(g2,1,1000) === g2
      connectByKey(g2,1000,1) === g2
      connectByKey(g2,1000,1000) === g2
      connectByKey(g2,1,5) === OldGraph(
        Vector(OldVertex(1,List(5,2,3,4)),OldVertex(2,List(1,3,4)),OldVertex(3,List(1,2,4)),OldVertex(4,List(1,2,3)),OldVertex(5,List(1))),
        Vector(OldEdge(1,2),OldEdge(1,3),OldEdge(1,4),OldEdge(2,3),OldEdge(2,4),OldEdge(3,4),OldEdge(1,5)))
    }

    "contract a vector of vertexes" in {
      contractVs(g.vs,0,1) === Vector(OldVertex(1,List(3,4,3,4)), OldVertex(3,List(1,1,4)), OldVertex(4,List(1,1,3)))
    }

    "contract a vector of edges" in {
      contractEs(g.es,g.vs,0,1) === Vector(OldEdge(1,3),OldEdge(1,4),OldEdge(1,3),OldEdge(1,4),OldEdge(3,4))
    }

    "contract a graph" in {
      val g2 = contract(g,0,1)
      g2 === OldGraph(
        Vector(OldVertex(1,List(3,4,3,4)), OldVertex(3,List(1,1,4)), OldVertex(4,List(1,1,3))),
        Vector(OldEdge(1,3),OldEdge(1,4),OldEdge(1,3),OldEdge(1,4),OldEdge(3,4)))
      sizes(g2) === (3,5)
    }

    "recursively and randomly contract a graph" in {
      sizes(randomContract(g))._1 === 2
    }

    "parse a graph from text input" in {
      parseTxt("src/test/resources/sampleGraph.txt") ===
        construct(Vector(
          OldVertex(1,List(2,3,4)),
          OldVertex(2,List(1,3,4)),
          OldVertex(3,List(1,2,4)),
          OldVertex(4,List(1,2,3))))
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
