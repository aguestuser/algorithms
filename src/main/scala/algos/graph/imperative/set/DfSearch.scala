package algos.graph.imperative.set

import scala.annotation.tailrec

/**
 * Author: @aguestuser
 * Date: 4/10/15
 */

case class DfAcc[A](explorers: List[Node[A]], res: DfRes[A])

sealed trait DfRes[A]
case class DfPath[A](p: List[Node[A]]) extends DfRes[A]
case class DfDist[A](p: Map[Node[A],Int]) extends DfRes[A]

object DfSearch {

  def explore[A](g: Graph[A], start: Node[A]): List[Node[A]] = {
    init(g,start)
    search(g, DfAcc[A](List(start),DfPath(List(start)))).res match {
      case DfPath(p) ⇒ p.reverse } }

  private def init[A](g: Graph[A], start: Node[A]): Unit = { init(g); init(start) }
  private def init[A](g: Graph[A]): Unit =  g.nodes foreach { _.explored = false }
  private def init[A](start: Node[A]): Unit = start.explored = true

  @tailrec
  private def search[A](g: Graph[A], acc: DfAcc[A]): DfAcc[A] =  acc match {
    case DfAcc(Nil,_) ⇒ acc
    case DfAcc(hd :: tail, res) ⇒
      search(g,(DfAcc(tail,res) /: hd.adj)(remember(g,hd)(_,_))) }

  def remember[A](g: Graph[A], n1: Node[A])(acc: DfAcc[A], n2: Node[A]): DfAcc[A] = {
    if (n2.explored) acc
    else { n2.explored = true; search(g,DfAcc(n2 :: acc.explorers, newResult(g,n1,n2,acc.res))) } }

  private def newResult[A](g: Graph[A], n1: Node[A], n2: Node[A], r: DfRes[A]): DfRes[A] = r match {
    case DfPath(p) ⇒ DfPath(n2 :: p)
    case DfDist(ds) ⇒ DfDist(ds + ((n2, ds(n1) + 1))) }

}
