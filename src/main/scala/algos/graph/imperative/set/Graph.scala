package algos.graph.imperative.set

/**
 * Author: @aguestuser
 * Date: 4/9/15
 */

case class Graph[A](nodes: Set[Node[A]])

object Graph {

  def add[A](g: Graph[A], n: Node[A]): Graph[A] = // O(1)
    Graph(g.nodes + n)

  def addMany[A](g: Graph[A], ns: List[Node[A]]): Graph[A] = // O(k) where k is # nodes being added
    (g /: ns)(add)

  def remove[A](g: Graph[A], n: Node[A]): Graph[A] = { // O(1)s
  val ns = g.nodes - n
    ns foreach { _.disconnect(n)}
    Graph(ns) }

  def removeMany[A](g: Graph[A], ns: List[Node[A]]): Graph[A] = // O(k) where k is #nodes being removed
    (g /: ns)(remove)

  def connect[A](g: Graph[A], e: Edge[A]): Graph[A] = // O(1)
    if (!g.nodes.contains(e.n1) || !g.nodes.contains(e.n2)) g
    else {
      e.n1.connect(e.n2)
      e.n2.connect(e.n1)
      g }

  def connectMany[A](g: Graph[A], es: List[Edge[A]]): Graph[A] = // O(1)
    (g /: es)(connect)

  def disconnect[A](g: Graph[A], e: Edge[A]): Graph[A] =
    if (!contains(g, List(e.n1, e.n2))) g
    else {
      e.n1.disconnect(e.n2)
      e.n2.disconnect(e.n1)
      g }

  def disconnectMany[A](g: Graph[A], es: List[Edge[A]]): Graph[A] = {
    es foreach { disconnect(g, _)}
    g }

  def contains[A](g: Graph[A], ns: List[Node[A]]): Boolean =
    (true /: ns)( _ && g.nodes.contains(_))
}

class Node[A](val item: A, var adj: Set[Node[A]] = Set[Node[A]]()) {

  def connect(n: Node[A]): Node[A] = { this.adj = this.adj + n; this } // O(1)
  def disconnect(n: Node[A]): Node[A] = { this.adj = this.adj - n; this } // O(1)

}

case class Edge[A](n1: Node[A], n2: Node[A])