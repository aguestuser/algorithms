package algos.graph.imperative.set

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

  //meta-functions

  def explore[A](g: Graph[A], start: Node[A]): List[Node[A]] = {
    init(g,start)
    search(BfAcc[A](Queue(start), BfPath(List(start)))).res match {
      case BfPath(p) ⇒ p.reverse } }

  def mapHops[A](g: Graph[A], start: Node[A]): Map[Int,Iterable[Node[A]]] = {
    init(g,start)
    search(BfAcc[A](Queue(start), BfDist(Map(start → 0)))).res match {
      case BfDist(d) ⇒ d.keys.groupBy(d(_)) } }

  def shortestPath[A](g: Graph[A], from: Node[A], to: Node[A]): Int = {
    init(g,from)
    search(BfAcc[A](Queue(from), BfDist(Map(from → 0)))).res match {
      case BfDist(ds) ⇒ ds(to) } }

  def connectedComponents[A](g: Graph[A]): Set[Set[Node[A]]] = {
    init(g)
    g.nodes map { connectedComponent } filter { _.size > 1 } }

  def connectedComponent[A](n: Node[A]): Set[Node[A]] = {
    init(n)
    search(BfAcc[A](Queue(n), BfPath(List(n)))).res match {
      case BfPath(p) ⇒ p.toSet } }

  private def init[A](g: Graph[A], start: Node[A]): Unit = { init(g); init(start) }
  private def init[A](g: Graph[A]): Unit =  g.nodes foreach { _.explored = false }
  private def init[A](start: Node[A]): Unit = start.explored = true

  //core functions

  private def search[A](acc: BfAcc[A]): BfAcc[A] = acc match { // O(n+e) where n is # nodes, e is # of edges
    case BfAcc(Queue(), _) ⇒ acc
    case BfAcc(exrs,r) ⇒
      val (n,exrs_) = exrs.dequeue // O(1)
      search((BfAcc(exrs_,r) /: n.adj)(examine(n)(_,_))) }

  private def examine[A](n1: Node[A])(acc: BfAcc[A], n2: Node[A]): BfAcc[A] = { // O(1)
    if (n2.explored) acc
    else {
      n2.explored = true
      record(n1,n2,acc) } }

  private def record[A](n1: Node[A], n2: Node[A], acc: BfAcc[A]): BfAcc[A] = {
    val res = acc.res match {
      case BfPath(p) ⇒ BfPath(n2 :: p)
      case BfDist(ds) ⇒ BfDist(ds + ((n2, ds(n1) + 1))) }
    BfAcc(acc.explorers.enqueue(n2),res) }

}
