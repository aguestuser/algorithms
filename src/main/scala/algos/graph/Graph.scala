package algos.graph

import scala.annotation.tailrec

/**
 * Author: @aguestuser
 * Date: 3/10/15
 * License: GPLv2
 */

//case class Node[A](item: A, adj: => collection.mutable.Set[Node[A]])
//case class Edge[A](src: Node[A], trg: Node[A])
//
//class Graph[A]{
//  var ns = Vector[Node[A]]()
//  var es = Vector[Edge[A]]()
//
//  def add(item: A): Graph[A] = {
//    ns = ns :+ Node(item,collection.mutable.Set[Node[A]]())
//    this
//  }
//  def connect(ui: Int, vi: Int): Graph[A] = {
//    ns(ui).adj += ns(vi)
//    ns(vi).adj += ns(ui)
//    es = es :+ Edge(ns(ui),ns(vi))
//    this
//  }
//}

class Node[A](val value: A) { var adj = List[Node[A]]() }
class Graph[A](var ns: Vector[Node[A]] = Vector()) {

  def addNode(value: A): Graph[A] = {
    ns = ns :+ new Node(value)
    this }

  def addNodes(values: List[A]): Graph[A] = {
    values map { addNode }
    this }

  def connect(i: Int, j: Int): Graph[A] = {
    val (n,o) = (ns(i),ns(j))
    n.adj = o :: n.adj
    this }

  def connectMany(indices: List[(Int,Int)]): Graph[A] = {
    indices map { case(i,j) => connect(i,j) }
    this }

  def disconnect(i: Int, j: Int): Graph[A] = {
    val (n,o) = (ns(i),ns(j))
    n.adj = n.adj filter { _ != o }
    o.adj = o.adj filter { _ != n }
    this }

  def contract(i: Int, j: Int): Graph[A] = {
    val (ni,nj) = (ns(i), ns(j))
    ni.adj = ni.adj.filter(_ != nj) ++ nj.adj.filter(_ != ni)
    ns = ns filter { _ != nj }
    this }

  def minCut: Int = {

    @tailrec
    def rContractMany(n: Int, min: Int): Int = {
      println(s"left: $n | cur min: $min")
      if (n == 0) min
      else rContractMany(n-1, math.min(min,rContract(
        new Graph[A]
          .addNodes(ns.toList.map(_.value))
          .connectMany(ns.toList flatMap { n => n.adj map { o => (ns.indexOf(n),ns.indexOf(o)) } })))) }

    def rContract(g: Graph[A]): Int = {
//      println(s"g.ns.size: ${g.ns.head.adj.size}")
      if (g.ns.size <= 2) g.ns.head.adj.size
      else { (g.contract _).tupled(rIndices(g)); rContract(g) } }
    def rIndices(g: Graph[A]): (Int,Int) = {
      val (a,b) = (rIndex(g),rIndex(g))
      if (a != b) (a,b) else rIndices(g) }
    def rIndex(g: Graph[A]): Int = (math.random * g.ns.size).toInt

    val x = ns.size
    rContractMany(x * x * math.log(x).toInt, ns.head.adj.size)
  }

//  def edges: Int = {
////    val ees = ns flatMap { n => n.adj map { nn => (n,nn) } }
//    ( 0 /: ns )((es,n) =>
//      n.adj map { o => if  }
//    )
//  }

}

object Graph {
  def parseTxt(path: String): Graph[Int] = {
    val lines: List[String] = io.Source.fromFile(path).getLines().toList
    val adjLists: List[List[Int]] = lines map { _.split("""[\t|\s]+""").toList.map(_.toInt) }
    val nodes: List[Int] = adjLists map { _.head }
    val indexLists: List[List[Int]] = adjLists map { al => al map { nv => nodes.indexOf(nv) } }
    val edges: List[(Int,Int)] = indexLists flatMap { case(hd::tl) => tl map { i => (hd,i) } }

    new Graph[Int].addNodes(nodes).connectMany(edges)
  }
}

//
//
//
//class Edge{}