package algos.graph.imperative.set

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * Author: @aguestuser
 * Date: 4/9/15
 */


case class BfAcc[A](explorers: Queue[Node[A]], res: BfRes[A])

sealed trait BfRes[A]
case class BfPath[A](p: List[Node[A]]) extends BfRes[A]
case class BfDist[A](d: Map[Node[A],Int]) extends BfRes[A]

object BfSearch {

  def explore[A](g: Graph[A], start: Node[A]): List[Node[A]] = {
    init(g,start)
    search(g, BfAcc[A](Queue(start), BfPath(List(start)))).res match {
      case BfPath(p) ⇒ p.reverse } }

  def mapHops[A](g: Graph[A], start: Node[A]): Map[Int,Iterable[Node[A]]] = {
    init(g,start)
    search(g, BfAcc[A](Queue(start), BfDist(Map(start → 0)))).res match {
      case BfDist(d) ⇒ d.keys.groupBy(d(_)) } }

  def shortestPath[A](g: Graph[A], from: Node[A], to: Node[A]): Int = {
    init(g,from)
    search(g, BfAcc[A](Queue(from), BfDist(Map(from → 0))))
      .res match { case BfDist(ds) ⇒ ds(to) } }

  def connectedComponents[A](g: Graph[A]): Set[Set[Node[A]]] = {
    init(g)
    g.nodes map { n ⇒
      init(n)
      search(g, BfAcc[A](Queue(n), BfPath(List(n)))).res match {
        case BfPath(p) ⇒ p.toSet }
    } filter { _.size > 1 } }

  private def init[A](g: Graph[A], start: Node[A]): Unit = { init(g); init(start) }
  private def init[A](g: Graph[A]): Unit =  g.nodes foreach { _.explored = false }
  private def init[A](start: Node[A]): Unit = start.explored = true

  @tailrec
  private def search[A](g: Graph[A], acc: BfAcc[A]): BfAcc[A] = acc match {
    // O(n+e) where n is # nodes, e is # of edges because:
      // n limits the # of nodes that get put in the `explorers` queue
      // e limits the total # of times that `remember` gets called
    case BfAcc(Queue(), _) ⇒ acc
    case BfAcc(exrs,r) ⇒
      val (n,exrs_) = exrs.dequeue // O(1)
      val acc_ = (BfAcc(exrs_,r) /: n.adj)(remember(g,n)(_,_)) // O(a) where a is # of adjacent nodes
      search(g,acc_) }

  private def remember[A](g: Graph[A], n1: Node[A])(acc: BfAcc[A], n2: Node[A]): BfAcc[A] = { // O(1)
    if (n2.explored) acc
    else {
      n2.explored = true
      BfAcc(
        acc.explorers.enqueue(n2),
        acc.res match {
          case BfPath(p) ⇒ rememberPath(g,n2, p)
          case BfDist(ds) ⇒ rememberDist(g,n1,n2,ds) } ) } }


  private def rememberPath[A](g: Graph[A], n: Node[A], path: List[Node[A]]): BfPath[A] = BfPath(n :: path) // O(1)
  private def rememberDist[A](g: Graph[A], n1: Node[A], n2: Node[A], ds: Map[Node[A],Int]): BfDist[A] = BfDist(ds + ((n2,ds(n1) + 1))) // O(1)

}
