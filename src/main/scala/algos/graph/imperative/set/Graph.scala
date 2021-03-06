package algos.graph.imperative.set

/**
 * Author: @aguestuser
 * Date: 4/9/15
 */

case class Graph[A](nodes: Set[Node[A]])

object Graph {

  def add[A](g: Graph[A], n: Node[A]): Graph[A] = Graph(g.nodes + n) // O(1)
  def addMany[A](g: Graph[A], ns: List[Node[A]]): Graph[A] = (g /: ns)(add) // O(1)

  def remove[A](g: Graph[A], n: Node[A]): Graph[A] = { // O(n) where n is # nodes in graph
    val ns = g.nodes - n; ns foreach { _.disconnect(n)}; Graph(ns) }
  def removeMany[A](g: Graph[A], ns: List[Node[A]]): Graph[A] = (g /: ns)(remove) // O(n)

  def connect[A](g: Graph[A], e: Edge[A]): Graph[A] = // O(1)
    if (!g.nodes.contains(e.n1) || !g.nodes.contains(e.n2)) g
    else { e.n1.connect(e.n2); e.n2.connect(e.n1); g}
  def connectMany[A](g: Graph[A], es: List[Edge[A]]): Graph[A] = (g /: es)(connect) // O(1)

  def disconnect[A](g: Graph[A], e: Edge[A]): Graph[A] = // O(1)
    if (!contains(g, List(e.n1, e.n2))) g
    else { e.n1.disconnect(e.n2); e.n2.disconnect(e.n1); g}
  def disconnectMany[A](g: Graph[A], es: List[Edge[A]]): Graph[A] = { es foreach { disconnect(g, _)}; g } // O(1)

  def contains[A](g: Graph[A], ns: List[Node[A]]): Boolean = (true /: ns)( _ && g.nodes.contains(_)) // O(x) where x is # nodes being checked
}

class Node[A](val item: A, var adj: Set[Node[A]] = Set[Node[A]](), var explored: Boolean = false) {

  def connect(n: Node[A]): Node[A] = { this.adj = this.adj + n; this } // O(1)
  def connectMany(ns: List[Node[A]]): Node[A] = (this /: ns)(_.connect(_))
  def disconnect(n: Node[A]): Node[A] = { this.adj = this.adj - n; this } // O(1)

}

case class Edge[A](n1: Node[A], n2: Node[A])