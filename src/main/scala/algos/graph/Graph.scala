package algos.graph

import scala.annotation.tailrec
import scala.math.random

/**
 * Author: @aguestuser
 * Date: 3/5/15
 * License: GPLv2
 */

case class Vertex[A](a: A, adj: List[A])
case class Edge[A](u: A, v: A)
case class Graph[A](vs: Vector[Vertex[A]], es: Vector[Edge[A]])

object Graph {

  import algos.graph.Vertex._
  import algos.graph.Edge._

  // construct
  def construct[A](vs: Vector[Vertex[A]])(implicit o: Ordering[A]): Graph[A] =
    Graph(vs,edges(vs))

  def parseTxt(path: String): Graph[Int] = {
    val lines = io.Source.fromFile(path).getLines().toVector
    val keys = lines map { _.split("""[\t|\s]+""").toList }
    val vs = keys map { x => Vertex(x.head.toInt, x.tail.map(_.toInt)) }
    construct(vs)
  }

  // modify vertexes
  def add[A](g: Graph[A], v: Vertex[A]): Graph[A] = {
    val es = v.adj.zipWithIndex map { case(a,i) => Edge(a,v.a) }
    Graph[A](g.vs :+ v, g.es ++ es) }

  def connect[A](g: Graph[A], i: Int, j: Int): Graph[A] = {
    val (vi, vj) = (g.vs(i),g.vs(j))
    //TODO add error handling here
    val (vvi,vvj) = (Vertex(vi.a, vj.a :: vi.adj), Vertex(vj.a, vi.a :: vj.adj))
    Graph[A](g.vs.updated(i, vvi).updated(j, vvj), g.es :+ Edge(vvi.a, vvj.a)) }
  
  def connectByKey[A](g: Graph[A], a: A, b: A): Graph[A] =
    (findV(g,a), findV(g,b)) match {
      case (None,_) => g
      case (_,None) => g
      case (Some(u),Some(v)) => connect(g,g.vs.indexOf(u),g.vs.indexOf(v)) }

  // contract
  def rContractN[A](g: Graph[A], n: Int)(implicit o: Ordering[A]): Int = {
    @tailrec
    def rContract1(g: Graph[A], n: Int, min: Int): Int =
      if (n == 0) min else rContract1(g, n-1, math.min(min,randomContract(g).es.size))
    rContract1(g,n,g.es.size) }

  @tailrec
  def randomContract[A](g: Graph[A])(implicit o: Ordering[A]): Graph[A] = {
    val gg = contract(g, (random * g.vs.size).toInt, (random * g.vs.size).toInt)
    if (gg.vs.size <= 2) gg else randomContract(gg) }

  def contract[A](g: Graph[A],i: Int, j: Int)(implicit o: Ordering[A]) =
    Graph[A](contractVs(g.vs,i,j),contractEs(g.es,g.vs,i,j))

  def contractVs[A](vs: Vector[Vertex[A]], i: Int, j: Int): Vector[Vertex[A]] =
    vs updated(i,combine(vs(i),vs(j))) map { Vertex.replace(_, vs(j).a, vs(i).a) } filter { _.a != vs(j).a }

  def contractEs[A](es: Vector[Edge[A]], vs: Vector[Vertex[A]], i: Int, j: Int): Vector[Edge[A]] =
    es map { Edge.replace(_,vs(j).a,vs(i).a) } filter { { case Edge(a,b) => a != b } }

  // utility
  def sizes[A](g: Graph[A]): (Int,Int) = (g.vs.size, g.es.size)
  def size[A](g: Graph[A]): Int = sizes(g) match { case(m,n) => m+n }
  def findV[A](g: Graph[A], a: A): Option[Vertex[A]] = g.vs find { _.a == a }
  def hasE[A](g: Graph[A], e: Edge[A]): Boolean = g.es contains { e }

}

object Vertex {
  def deg[A](v: Vertex[A]): Int = v.adj.size
  def connected[A](u: Vertex[A], v: Vertex[A]): Boolean = u.adj.contains(v)
  def combine[A](u: Vertex[A], v: Vertex[A]): Vertex[A] = Vertex(u.a, u.adj.filter(_ != v.a) ::: v.adj.filter(_ != u.a))
  def replace[A](v: Vertex[A], a: A, b: A): Vertex[A] = Vertex(v.a, v.adj.map(x => if (x == a) b else x))
}

object Edge {

  def edges[A](vs: Vector[Vertex[A]])(implicit o: Ordering[A]): Vector[Edge[A]] =
    vs.flatMap(v => v.adj.toVector.map(a => (v.a,a)))
      .map(order(_)).distinct.map(p => Edge(p._1,p._2))

  def order[A](as:(A,A))(implicit o: Ordering[A]):(A,A) = as match {
    case(a,aa) => if (o.lteq(a,aa)) (a,aa) else (aa,a) }

  def replace[A](e: Edge[A], a: A, b: A): Edge[A] = {
    def replace1(x: A): A = if (x == a) b else x
    e match { case Edge(u,v) => Edge(replace1(u),replace1(v)) }
  }

}