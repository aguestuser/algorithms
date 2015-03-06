package algos.graph

import algos.graph.Edge._

/**
 * Author: @aguestuser
 * Date: 3/5/15
 * License: GPLv2
 */

case class Graph[A](vs: Vector[Vertex[A]], es: Vector[Edge[A]])
case class Vertex[A](a: A, adj: List[A])
case class Edge[A](u: A, v: A)

object Graph {

  def construct[A](vs: Vector[Vertex[A]]): Graph[A] = Graph(vs,edges(vs))

  def add[A](g: Graph[A], v: Vertex[A]): Graph[A] = {
    val es = v.adj.zipWithIndex map { case(a,i) => Edge(a,v.a) }
    Graph[A](g.vs :+ v, g.es ++ es) }

  def connect[A](g: Graph[A], u: Vertex[A], v: Vertex[A]): Graph[A] = {
    val (uu,vv) = (Vertex(u.a, v.a :: u.adj), Vertex(v.a, u.a :: v.adj))
    val (ui, vi) = (g.vs.indexOf(u), g.vs.indexOf(v))
    val ee = Edge(uu.a, vv.a)
    Graph[A](g.vs.updated(ui, uu).updated(vi, vv), g.es :+ ee) }

  def sizes[A](g: Graph[A]): (Int,Int) = (g.vs.size, g.es.size)
  def size[A](g: Graph[A]): Int = sizes(g) match { case(m,n) => m+n }

  def findv[A](g: Graph[A], a: A): Option[Vertex[A]] = g.vs find { _.a == a }
  def hasv[A](g: Graph[A], a: A): Boolean = g.vs exists { _.a == a }

  def finde[A](g: Graph[A], e: Edge[A]): Option[Edge[A]] = g.es find { _ == e }
  def hase[A](g: Graph[A], e: Edge[A]): Boolean = g.es contains { e }

}

object Vertex {
  def deg[A](v: Vertex[A]): Int = v.adj.size
  def connected[A](u: Vertex[A], v: Vertex[A]): Boolean = u.adj.contains(v)
}


object Edge {

  def edges[A](vs: Vector[Vertex[A]])(implicit o: Ordering[A]): Vector[Edge[A]] =
    vs.flatMap(v => v.adj.toVector.map(a => (v.a,a)))
      .map(order(_)).distinct.map(p => Edge(p._1,p._2))

  def order[A](as:(A,A))(implicit o: Ordering[A]):(A,A) = as match {
    case(a,aa) => if (o.lteq(a,aa)) (a,aa) else (aa,a) }

//  def equiv[A](e1: Edge[A], e2: Edge[A]): Boolean = (e1,e2) match {
//    case (Edge(u1,v1),(u2,v2)) => (u1 == u2 && v1 == v2) || (u1 == v2 && v1 == u2) }
}