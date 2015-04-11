package algos.graph.imperative.map

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * Author: @aguestuser
 * Date: 4/9/15
 */

case class BfAcc[A](explorers: Queue[Node[A]], explored: Set[Node[A]], res: BfRes[A])

sealed trait BfRes[A]
case class BfList[A](list: List[Node[A]]) extends BfRes[A]
case class BfMap[A](d: Map[Node[A],Int]) extends BfRes[A]

object BfSearch {

  def bfExplore[A](g: Graph[A], start: Node[A]): List[Node[A]] =
    bfSearch(g,
      BfAcc[A](
        Queue(start),
        Set(start),
        BfList(Nil)))
      .res match { case BfList(p) ⇒ p.reverse }

  def bfShortestPath[A](g: Graph[A], from: Node[A], to: Node[A]): Int =
    bfSearch(g,
      BfAcc[A](
        Queue(from),
        Set(from),
        BfMap(Map(from → 0))))
      .res match { case BfMap(ds) ⇒ ds(to) }

  @tailrec
  private def bfSearch[A](g: Graph[A], acc: BfAcc[A]): BfAcc[A] = acc match { // O(n+e) where n is # nodes, e is # of edges
    case BfAcc(Queue(), _, _) ⇒ acc
    case BfAcc(exrs,exd,r) ⇒
      val (n,exrs_) = exrs.dequeue // O(1)
      val acc_ = (BfAcc(exrs_,exd,r) /: n.adj)(remember(g,n)(_)(_)) // O(a) where a is # of adjacent nodes
      bfSearch(g,acc_) }

  def remember[A](g: Graph[A], n1: Node[A])(acc: BfAcc[A])(n2: Node[A]): BfAcc[A] = { // O(1)
    if (acc.explored.contains(n2)) acc
    else BfAcc(
      acc.explorers.enqueue(n2),
      acc.explored + n2,
      acc.res match {
        case BfList(p) ⇒ rememberPath(g,n2, p)
        case BfMap(ds) ⇒ rememberDist(g,n1,n2,ds) } ) }

  def rememberPath[A](g: Graph[A], n: Node[A], path: List[Node[A]]): BfList[A] = BfList(n :: path) // O(1)
  def rememberDist[A](g: Graph[A], n1: Node[A], n2: Node[A], ds: Map[Node[A],Int]): BfMap[A] = BfMap(ds + ((n2,ds(n1) + 1))) // O(1)

}