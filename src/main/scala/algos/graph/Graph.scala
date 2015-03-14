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

  def findIndex[A](g: Graph[A], v: A): Int = nodeIndex(g,findNode(g,v).get)
  def findNode[A](g: Graph[A], v: A): Option[Node[A]] = g.ns.find(_.v == v)
  def nodeIndex[A](g: Graph[A], n: Node[A]): Int = g.ns.indexOf(n)

  def connect[A](g: Graph[A], i: Int, j: Int): Graph[A] = {
    val (ni,nj) = (g.ns(i),g.ns(j))
    Graph(g.ns
      .updated(i,Node(ni.v, nj.v :: ni.adj))
      .updated(j,Node(nj.v, ni.v :: nj.adj)))}

  def connectMany[A](g: Graph[A], indices: List[(Int,Int)]): Graph[A] = {
    (g /: indices)({case(gg,(i,j)) => connect(gg,i,j)})}

  def minCut[A](g: Graph[A]): Int = {
    @tailrec
    def rContractMany(n: Int, min: Int): Int = {
      if (n == 0) min
      else rContractMany(n-1, math.min(min,rContractOne(g))) }
    rContractMany(g.ns.size, g.ns(0).adj.size) }

  private def rContractOne[A](g: Graph[A]): Int = //{
    if (g.ns.size == 2) g.ns.head.adj.size
    else rIndices(g) match { case(i,j) => rContractOne(contract(g,i,j)) }

  private def rIndices[A](g: Graph[A]): (Int,Int) = {
    val i = rand(g.ns.size)
    val adj = g.ns(i).adj
    val j = findIndex(g,adj(rand(adj.size)))
    (i, j) }

  private def rand(i: Int): Int = (math.random * i).toInt

  def contract[A](g: Graph[A], i: Int, j: Int): Graph[A] = {
    if (i == j) g // can't contract a node on itself
    else {
      val (ni,nj) = (g.ns(i),g.ns(j))
      Graph(g.ns
        .updated(i,Node.combine(ni,nj))
        .filter(_ != nj)
        .map(Node.replace(_,nj.v,ni.v))) } }

  def parseTxt(path: String): Graph[Int] = {
    val lines = io.Source.fromFile(path).getLines().toVector
    val keyLists = lines map { _.split("""[\t|\s]+""").toList }
    val ns = keyLists map { kl => Node(kl.head.toInt, kl.tail.map(_.toInt)) }
    Graph(ns) }
}

object Node {

  def combine[A](n: Node[A], m: Node[A]): Node[A] =
    Node(n.v, n.adj.filter(_ != m.v) ::: m.adj.filter(_ != n.v))

  def replace[A](n: Node[A], targ: A, repl: A): Node[A] = n match {
    case Node(v,a) =>
      val vv = if (v == targ) repl else v
      val aa = a map { vvv => if (vvv == targ) repl else vvv }
      Node(vv,aa) }
}