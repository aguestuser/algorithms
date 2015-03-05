package algos.graph

import algos.sort.Sort._

/**
 * Author: @aguestuser
 * Date: 3/5/15
 * License: GPLv2
 */

case class Graph[A](vs: Vector[Vertex[A]], es: Vector[Edge[A]])
case class Vertex[A](i: Int, a: A, adj: List[Vertex[A]])
case class Edge[A](i: Int, u: Vertex[A], v: Vertex[A])

object gOrd extends Ordering[Graph] {def compare(f: Graph, g: Graph) = Graph.size(f) compare Graph.size(g) }
object vOrd extends Ordering[Vertex] {
  def compare(u: Vertex, v: Vertex) = u.i compare v.i
}
object eOrd extends Ordering[Edge] {
  def compare(d: Edge, e: Edge) = d.i compare e.i
  override def equiv(d: Edge, e: Edge) =
    d.i == e.i && ((d.u == e.u && d.v == d.v) || (d.u == e.v && d.v == d.u))
}

object Graph {

  def size(g: Graph): Int = ???
  
  def add[A](g: Graph[A], a: A, adj: List[Vertex[A]]): Graph[A] = {
    val u = Vertex(g.vs.size, a, adj)
    val es = adj.zipWithIndex map { case(v,i) => Edge(g.vs.size + i,u,v) }
    Graph(g.vs :+ u, g.es ++ es) }

  def connect[A](g: Graph[A], u: Vertex[A], v: Vertex[A]): Graph[A] = {
    val (uu,vv) = (Vertex(u.i, u.a, v :: u.adj), Vertex(v.i, v.a, u :: v.adj))
    val ee = Edge(g.es.size, uu, vv)
    Graph(g.vs.updated(u.i, uu).updated(v.i, vv), g.es.updated(ee.i, ee)) }

  def vertex[A](g: Graph[A], i: Int): Vertex[A] = g.vs(i)
  def hasVertex[A](g: Graph[A], v: Vertex)(implicit o: Ordering[Vertex]): Boolean =

  def edge[A](g: Graph[A], i: Int): Edge[A] = g.es(i)
  def hasEdge[A](g: Graph[A], v: Vertex): Boolean = ???

}

object Vertex {
  def degree(v: Vertex): Int = v.adj.size
  def connected(u: Vertex, v: Vertex): Boolean = u.adj.contains(v)
}



object Edge {

  def edges[A](vs: Vector[Vertex[A]]): Vector[Edge[A]] = ???
}