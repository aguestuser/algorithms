package algos.graph.imperative


/**
 * Author: @aguestuser
 * Date: 4/6/15
 */

case class Graph[A](nodes: Vector[Node[A]])

object Graph {

  def add[A](g: Graph[A], n: Node[A]): Graph[A] = // O(1)
    Graph(g.nodes :+ n)

  def addMany[A](g: Graph[A], ns: Vector[Node[A]]) = // O(1)
    Graph(g.nodes ++ ns)

  def remove[A](g: Graph[A], n: Node[A]): Graph[A] = {// O(n)
    val ns = g.nodes.filter(_ != n) // O(n)
    ns foreach { nn ⇒ nn.adj = nn.adj - n } // O(n)
    Graph(ns) }

  def removeMany[A](g: Graph[A], ns: List[Node[A]]): Graph[A] = // O(k*n) where k is number of nodes being removed, n is # nodes in graph
    (g /: ns)((gg,n) ⇒ remove(gg,n))

  def connect[A](g: Graph[A], e: Edge[A]): Graph[A] =  // O(n) where n is nodes.size
    connectByIndex(g, g.nodes.indexOf(e.n1), g.nodes.indexOf(e.n2))

  def connectByIndex[A](g: Graph[A], i: Int, j: Int): Graph[A] = // O(1)
    Graph(g.nodes
      .updated(i, g.nodes(i).connect(g.nodes(j)))
      .updated(j, g.nodes(j).connect(g.nodes(i))))

  def connectMany[A](g: Graph[A], es: List[Edge[A]]) = // O(n*m) where n is # nodes, m is # edges
    (g /: es)((gg,e) ⇒ connect(gg,e))

  def connectManyByIndex[A](g: Graph[A], is: List[(Int,Int)]): Graph[A] = // O(m) where m is # edges
    (g /: is)({ case(gg,(i,j)) ⇒ connectByIndex(gg,i,j)})

  def disconnect[A](g: Graph[A], e: Edge[A]): Graph[A] =
    disconnectByIndex(g, g.nodes.indexOf(e.n1), g.nodes.indexOf(e.n2))

  def disconnectByIndex[A](g: Graph[A], i: Int, j: Int): Graph[A] =
    Graph(g.nodes
      .updated(i, g.nodes(i).disconnect(g.nodes(j)))
      .updated(j, g.nodes(j).disconnect(g.nodes(i))))

}

class Node[A](val item: A, var adj: Set[Node[A]] = Set[Node[A]]()) {
  def connect(n: Node[A]): Node[A] = { this.adj = this.adj + n; this }
  def disconnect(n: Node[A]): Node[A] = { this.adj = this.adj - n; this }
}

case class Edge[A](n1: Node[A], n2: Node[A])