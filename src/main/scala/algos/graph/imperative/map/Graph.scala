package algos.graph.imperative.map

/**
 * Author: @aguestuser
 * Date: 4/9/15
 */

case class Graph[A](nodes: Map[Node[A],Node[A]])

object Graph {

  def add[A](g: Graph[A], n: Node[A]): Graph[A] = // O(1)
    Graph(g.nodes + (n → n))

  def addMany[A](g: Graph[A], ns: List[Node[A]]): Graph[A] = // O(k) where k is # nodes being added
    Graph(g.nodes ++  ns.map { n ⇒ n → n } )

  def remove[A](g: Graph[A], n: Node[A]): Graph[A] = { // O(1)
    val ns = g.nodes - n
    ns.values foreach { _.disconnect(n) }
    Graph(ns) }

  def removeMany[A](g: Graph[A], ns: List[Node[A]]): Graph[A] = { // O(k) where k is #nodes being removed
    (g /: ns)((gg,n) ⇒ remove(gg,n)) }

  def connect[A](g: Graph[A], e: Edge[A]): Graph[A] = { // O(1)
    g.nodes(e.n1).connect(e.n2)
    g.nodes(e.n2).connect(e.n1)
    g }

  def connectMany[A](g: Graph[A], es: List[Edge[A]]): Graph[A] = { // O(1)
    es foreach { connect(g,_) }
    g }

  def disconnect[A](g: Graph[A], e: Edge[A]): Graph[A] = {
    g.nodes(e.n1).disconnect(e.n2)
    g.nodes(e.n2).disconnect(e.n1)
    g }

  def disconnectMany[A](g: Graph[A], es: List[Edge[A]]): Graph[A] = {
    es foreach { disconnect(g,_) }
    g }

}

class Node[A](val item: A, var adj: Set[Node[A]] = Set[Node[A]]()) {

  def connect(n: Node[A]): Node[A] = { this.adj = this.adj + n; this } // O(1)
  def disconnect(n: Node[A]): Node[A] = { this.adj = this.adj - n; this } // O(1)

}

case class Edge[A](n1: Node[A], n2: Node[A])