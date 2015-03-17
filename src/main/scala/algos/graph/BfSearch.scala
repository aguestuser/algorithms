package algos.graph

import algos.graph.Graph._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * Author: @aguestuser
 * Date: 3/17/15
 * License: GPLv3
 */

case class BfAcc[A](q: Queue[Int], found: Set[Int], res: BfRes[A])

sealed trait BfRes[A]
case class BfPath[A](p: List[Node[A]]) extends BfRes[A]
case class BfDist[A](ds: Map[/*index*/Int,/*dist*/Int]) extends BfRes[A]

object BfSearch {

  def bfExplore[A](g: Graph[A], n: Node[A]): List[Node[A]] = bfExplore(g, findIndex(g, n.v))
  def bfExplore[A](g: Graph[A], in: Int): List[Node[A]] =
    bfSearch(g, BfAcc(Queue(in), Set(in), BfPath(Nil))).res match { case BfPath(p) ⇒ p.reverse }
  def bfShortestPath[A](g: Graph[A], from: Int, to: Int): Int =
    bfSearch(g, BfAcc(Queue(from),Set(from),BfDist(Map((from,0))))).res match { case BfDist(ds) ⇒ ds(to) }

  @tailrec
  private def bfSearch[A](g: Graph[A], acc: BfAcc[A]): BfAcc[A] = acc match {
    case BfAcc(Queue(), _, _) ⇒ acc
    case BfAcc(q, f, r) ⇒
      val (i, qq) = q.dequeue
      val acc_ = (BfAcc(qq,f,r) /: adjEdges(g,i))(remember(g)(_)(_)) // CALL TO adjEdges BLOWS THE BIG-O!!! :(
      bfSearch(g,acc_) }

  private def remember[A](g: Graph[A])(acc: BfAcc[A])(edge: (Int,Int)): BfAcc[A] = (acc,edge) match {
    case (BfAcc(q,f,r),(src,trg)) ⇒
      if (f.contains(trg)) acc
      else BfAcc(q :+ trg, f + trg, r match {
        case BfPath(p) ⇒ rememberPath(g,trg,p)
        case BfDist(ds) ⇒ rememberDist(g,src,trg,ds) } ) }
  
  private def rememberPath[A](g: Graph[A], i: Int, p: List[Node[A]]): BfPath[A] = BfPath(g.ns(i) :: p)
  private def rememberDist[A](g: Graph[A], src: Int, trg: Int, ds: Map[Int,Int]): BfDist[A] = BfDist(ds + ((trg,ds(src) + 1)))
    
}
