package algos.graph

/**
 * Author: @aguestuser
 * Date: 3/5/15
 * License: GPLv2
 */

//trait Graph {
//  def neighbors(v: Int): List[Int]
//  def add(u: Int, v: Int): Graph
//  def adjacent(u: Int, v: Int): Boolean = neighbors(u).contains(v)
//  def degree(v: Int): Int = neighbors(v).size
//}

case class Graph[A](vs: Vector[Vertex[A]], es: Vector[Edge[A]])
case class Edge[A](i: Int, u: Vertex[A], v: Vertex[A])
case class Vertex[A](i: Int, v: A, adj: List[Vertex[A]])

object Graph {
  def deg(v: Vertex): Int = v.adj.size
  def connected(u: Vertex, v: Vertex): Boolean = u.adj.contains(v)
  def connect[A](g: Graph[A], u: Vertex[A], v: Vertex[A]): Graph[A] = {
    val (uu,vv) = (Vertex(u.i, u.v, v :: u.adj), Vertex(v.i, v.v, u :: v.adj))
    val ee = Edge(g.es.size, uu, vv)
    Graph(g.vs.updated(u.i, uu).updated(v.i, vv), g.es.updated(ee.i, ee)) }


}
