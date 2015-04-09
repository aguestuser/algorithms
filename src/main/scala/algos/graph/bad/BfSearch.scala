package algos.graph.bad

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import Graph._

/**
 * Author: @aguestuser
 * Date: 4/6/15
 */

case class BfAcc[A](q: Queue[Int], found: Set[Int], res: BfRes[A])

sealed trait BfRes[A]
case class Path[A](p: List[Node[A]]) extends BfRes[A]
case class Dist[A](ds: Map[/*index*/Int,/*dist*/Int]) extends BfRes[A]
case class HopMap[A](ds: Map[/*dist*/Int,Node[A]]) extends BfRes[A]

object BfSearch {

  def bfExplore[A](g: Graph[A], n: Node[A]): List[Node[A]] = bfExplore(g, findIndex(g, n.v))
  def bfExplore[A](g: Graph[A], in: Int): List[Node[A]] =
    bfSearch(g, BfAcc(Queue(in), Set(in), Path(Nil))).res match { case Path(p) ⇒ p.reverse }

  def bfShortestPath[A](g: Graph[A], from: Int, to: Int): Int =
    bfSearch(g, BfAcc(Queue(from),Set(from),Dist(Map((from,0))))).res match {
      case Dist(ds) ⇒ ds(to) }

//  def bfSortByHops[A](g: Graph[A], in: Int): Map[Int,Node[A]] =
//    bfSearch(g, BfAcc(Queue(in),Set(in),Dist(Map((in,0))))).res match {
//      case HopMap(ds,hs) ⇒ hs
//    }

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
        case Path(p) ⇒ rememberPath(g, trg, p)
        case Dist(ds) ⇒ rememberDist(g, src, trg, ds) } ) }

  private def rememberPath[A](g: Graph[A], i: Int, p: List[Node[A]]): Path[A] = Path(g.ns(i) :: p)
  private def rememberDist[A](g: Graph[A], src: Int, trg: Int, ds: Map[Int,Int]): Dist[A] = Dist(ds + ((trg,ds(src) + 1)))
//  private def rememberDistToIndex[A](g: Graph[A], src: Int, trg: Int, ds: Map[Int,Node[A]])
//  : HopMap[A] = HopMap(ds + ((ds(src) + 1,g.ns(trg))))

}
