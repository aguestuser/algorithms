package algos.graph

import scala.annotation.tailrec

/**
 * Author: @aguestuser
 * Date: 3/10/15
 * License: GPLv2
 */

case class Node[A](v: A, adj: List[A])
case class Graph[A](ns: Vector[Node[A]])



object Graph {

  def findIndex[A](g: Graph[A], v: A): Int = g.ns.zipWithIndex.find({ case(n,i) ⇒ n.v == v}).get._2 // O(n) where in is # nodes
  def adjIndices[A](g: Graph[A], i: Int): List[Int] = g.ns(i).adj.map(n ⇒ findIndex(g,n)) // O(n*m)
  def adjEdges[A](g: Graph[A], i: Int): List[(Int,Int)] = adjIndices(g,i) map { (i,_) } // O(n*m)

  def connect[A](g: Graph[A], i: Int, j: Int): Graph[A] = { // O(1)
    val (ni,nj) = (g.ns(i),g.ns(j))
    Graph(g.ns
      .updated(i,Node(ni.v, nj.v :: ni.adj))
      .updated(j,Node(nj.v, ni.v :: nj.adj)))}

  def connectMany[A](g: Graph[A], indices: List[(Int,Int)]): Graph[A] = { // O(n) where n is indices.size
    (g /: indices)({case(gg,(i,j)) => connect(gg,i,j)})}

  def minCut[A](g: Graph[A]): Int = { // O(n^3 * m) where n is # nodes, m is # edges; m = n^2 in worst case so O(n^5)
    @tailrec
    def rContractMany(n: Int, min: Int): Int = {
      if (n == 0) min
      else rContractMany(n-1, math.min(min,rContractOne(g))) } // O(n^2 * m) where n is # nodes, m is # edges [runs n times]
    rContractMany(g.ns.size, g.ns(0).adj.size) }

  private def rContractOne[A](g: Graph[A]): Int = //{ O(n^2 * m) where n is g.ns.size, m is max size of any node's adj list
    if (g.ns.size == 2) g.ns.head.adj.size
    else rIndices(g) match { case(i,j) ⇒ rContractOne(contract(g,i,j)) } // O(n*m)

  private def rIndices[A](g: Graph[A]): (Int,Int) = { // O(n^2) where is is g.ns.size
    val i = rand(g.ns.size) // O(1)
    val adj = g.ns(i).adj // O(1)
    val j = findIndex(g,adj(rand(adj.size))) // O(n^2) where n is g.ns.size
    (i, j) }

  private def rand(i: Int): Int = (math.random * i).toInt // O(1)

  def contract[A](g: Graph[A], i: Int, j: Int): Graph[A] = { // O(n + m) where n is # nodes, m is # of edges
    if (i == j) g // can't contract a node on itself
    else {
      val (ni,nj) = (g.ns(i),g.ns(j)) // O(1)
      Graph(g.ns
        .updated(i,Node.combine(ni,nj)) // O(k) where k is # of ni.adj.size + nj.adj.size
        .filter(_ != nj) // O(n) where n is g.ns.size
        .map(Node.replace(_,nj.v,ni.v))) } } // O(n + m) where n is g.ns.size, m is # of edges

  def parseTxt(path: String): Graph[Int] = {
    val lines = io.Source.fromFile(path).getLines().toVector
    val keyLists = lines map { _.split("""[\t|\s]+""").toList }
    val ns = keyLists map { kl ⇒ Node(kl.head.toInt, kl.tail.map(_.toInt)) }
    Graph(ns) }
}

object Node {

  def combine[A](n: Node[A], m: Node[A]): Node[A] = // O(m) where n is n.adj.size + m.adj.size
    Node(n.v, n.adj.filter(_ != m.v) ::: m.adj.filter(_ != n.v))

  def replace[A](n: Node[A], targ: A, repl: A): Node[A] = n match { // O(m) where m is n.adj size
    case Node(v,a) =>
      val vv = if (v == targ) repl else v
      val aa = a map { vvv => if (vvv == targ) repl else vvv }
      Node(vv,aa) }
}