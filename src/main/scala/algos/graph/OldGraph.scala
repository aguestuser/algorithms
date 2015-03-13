package algos.graph

import scala.annotation.tailrec
import scala.math.random

/**
 * Author: @aguestuser
 * Date: 3/5/15
 * License: GPLv2
 */

case class OldVertex[A](a: A, adj: List[A])
case class OldEdge[A](u: A, v: A)
case class OldGraph[A](vs: Vector[OldVertex[A]], es: Vector[OldEdge[A]])

object OldGraph {

  import algos.graph.OldVertex._
  import algos.graph.OldEdge._

  // construct
  def construct[A](vs: Vector[OldVertex[A]])(implicit o: Ordering[A]): OldGraph[A] =
    OldGraph(vs,edges(vs))

  def parseTxt(path: String): OldGraph[Int] = {
    val lines = io.Source.fromFile(path).getLines().toVector
    val keys = lines map { _.split("""[\t|\s]+""").toList }
    val vs = keys map { x => OldVertex(x.head.toInt, x.tail.map(_.toInt)) }
    construct(vs)
  }

  // modify vertexes
  def add[A](g: OldGraph[A], v: OldVertex[A]): OldGraph[A] = {
    val es = v.adj.zipWithIndex map { case(a,i) => OldEdge(a,v.a) }
    OldGraph[A](g.vs :+ v, g.es ++ es) }

  def connect[A](g: OldGraph[A], i: Int, j: Int): OldGraph[A] = {
    val (vi, vj) = (g.vs(i),g.vs(j))
    //TODO add error handling here
    val (vvi,vvj) = (OldVertex(vi.a, vj.a :: vi.adj), OldVertex(vj.a, vi.a :: vj.adj))
    OldGraph[A](g.vs.updated(i, vvi).updated(j, vvj), g.es :+ OldEdge(vvi.a, vvj.a)) }
  
  def connectByKey[A](g: OldGraph[A], a: A, b: A): OldGraph[A] =
    (findV(g,a), findV(g,b)) match {
      case (None,_) => g
      case (_,None) => g
      case (Some(u),Some(v)) => connect(g,g.vs.indexOf(u),g.vs.indexOf(v)) }

  // contract
  def rContractN[A](g: OldGraph[A], n: Int)(implicit o: Ordering[A]): Int = {
    @tailrec
    def rContract1(g: OldGraph[A], n: Int, min: Int): Int =
      if (n == 0) min else rContract1(g, n-1, math.min(min,randomContract(g).es.size))
    rContract1(g,n,g.es.size) }

  @tailrec
  def randomContract[A](g: OldGraph[A])(implicit o: Ordering[A]): OldGraph[A] = {
    val gg = contract(g, (random * g.vs.size).toInt, (random * g.vs.size).toInt)
    if (gg.vs.size <= 2) gg else randomContract(gg) }

  def contract[A](g: OldGraph[A],i: Int, j: Int)(implicit o: Ordering[A]) =
    OldGraph[A](contractVs(g.vs,i,j),contractEs(g.es,g.vs,i,j))

  def contractVs[A](vs: Vector[OldVertex[A]], i: Int, j: Int): Vector[OldVertex[A]] =
    vs updated(i,combine(vs(i),vs(j))) map { OldVertex.replace(_, vs(j).a, vs(i).a) } filter { _.a != vs(j).a }

  def contractEs[A](es: Vector[OldEdge[A]], vs: Vector[OldVertex[A]], i: Int, j: Int): Vector[OldEdge[A]] =
    es map { OldEdge.replace(_,vs(j).a,vs(i).a) } filter { { case OldEdge(a,b) => a != b } }

  // utility
  def sizes[A](g: OldGraph[A]): (Int,Int) = (g.vs.size, g.es.size)
  def size[A](g: OldGraph[A]): Int = sizes(g) match { case(m,n) => m+n }
  def findV[A](g: OldGraph[A], a: A): Option[OldVertex[A]] = g.vs find { _.a == a }
  def hasE[A](g: OldGraph[A], e: OldEdge[A]): Boolean = g.es contains { e }

}

object OldVertex {
  def deg[A](v: OldVertex[A]): Int = v.adj.size
  def connected[A](u: OldVertex[A], v: OldVertex[A]): Boolean = u.adj.contains(v)
  def combine[A](u: OldVertex[A], v: OldVertex[A]): OldVertex[A] = OldVertex(u.a, u.adj.filter(_ != v.a) ::: v.adj.filter(_ != u.a))
  def replace[A](v: OldVertex[A], a: A, b: A): OldVertex[A] = OldVertex(v.a, v.adj.map(x => if (x == a) b else x))
}

object OldEdge {

  def edges[A](vs: Vector[OldVertex[A]])(implicit o: Ordering[A]): Vector[OldEdge[A]] =
    vs.flatMap(v => v.adj.toVector.map(a => (v.a,a)))
      .map(order(_)).distinct.map(p => OldEdge(p._1,p._2))

  def order[A](as:(A,A))(implicit o: Ordering[A]):(A,A) = as match {
    case(a,aa) => if (o.lteq(a,aa)) (a,aa) else (aa,a) }

  def replace[A](e: OldEdge[A], a: A, b: A): OldEdge[A] = {
    def replace1(x: A): A = if (x == a) b else x
    e match { case OldEdge(u,v) => OldEdge(replace1(u),replace1(v)) }
  }

}