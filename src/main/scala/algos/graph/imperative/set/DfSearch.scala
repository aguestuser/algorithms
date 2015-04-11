package algos.graph.imperative.set

import scala.annotation.tailrec

/**
 * Author: @aguestuser
 * Date: 4/10/15
 */

case class DfAcc[A](explorers: List[Node[A]], res: DfRes[A])

sealed trait DfRes[A]
case class DfPath[A](list: List[Node[A]]) extends DfRes[A]
case class DfTopo[A](map: Map[Int,Node[A]], label: Int) extends DfRes[A]

object DfSearch {

  // meta-functions

  def explore[A](g: Graph[A], start: Node[A]): List[Node[A]] = {
    init(g,start)
    search(DfAcc[A](List(start),DfPath(List(start))))
      .res match { case DfPath(l) ⇒ l.reverse } }

  def topoSort[A](g: Graph[A]): Map[Int,Node[A]] = {
    init(g)
    (DfAcc[A](Nil,DfTopo[A](Map(), g.nodes.size)) /: g.nodes)(topoSortOne)
      .res match { case DfTopo(m,_) ⇒ m } }

  def topoSortOne[A](acc: DfAcc[A], n: Node[A]): DfAcc[A] =
    if (n.explored) acc
    else { init(n); search(DfAcc[A](List(n),acc.res)) }

  private def init[A](g: Graph[A], start: Node[A]): Unit = { init(g); init(start) }
  private def init[A](g: Graph[A]): Unit =  g.nodes foreach { _.explored = false }
  private def init[A](start: Node[A]): Unit = start.explored = true

  // core functions

  @tailrec
  private def search[A](acc: DfAcc[A]): DfAcc[A] =  acc match {
    case DfAcc(Nil,_) ⇒ acc
    case DfAcc(hd :: _, res) ⇒
      search(examine(hd,hd.adj.toList,acc)) }

  def examine[A](n1: Node[A], n2s: List[Node[A]], acc: DfAcc[A]): DfAcc[A] = n2s match {
    case Nil ⇒ retreat(acc,n1)
    case hd :: tail ⇒
      if (hd.explored)
        examine(n1,tail,acc)
      else {
        hd.explored = true
        advance(acc,hd) } }

  private def advance[A](acc: DfAcc[A], n: Node[A]): DfAcc[A] = {
    val res = acc.res match {
      case DfPath(p) ⇒ DfPath(n :: p)
      case _ ⇒ acc.res }
    search(DfAcc(n :: acc.explorers, res)) }

  private def retreat[A](acc: DfAcc[A], n: Node[A]): DfAcc[A] = {
    val res = acc.res match {
      case DfTopo(m,l) ⇒ DfTopo(m + (l → n), l - 1)
      case _ ⇒ acc.res }
    DfAcc(acc.explorers.tail,res)
  }
}