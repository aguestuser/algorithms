package algos.sort

import scala.annotation.tailrec

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */


object Sort {

  def mSort[A](as: List[A])(implicit o: Ordering[A]): List[A] = {

    @tailrec
    def merge(l: List[A], r: List[A], acc: List[A]): List[A] = (l, r) match {
      case (Nil, _) => r.reverse ::: acc
      case (_, Nil) => l.reverse ::: acc
      case (_,_) =>
        if (o.compare(l.head, r.head) <= 0) merge(l.tail, r, l.head :: acc)
        else merge(l, r.tail, r.head :: acc) }

    as match {
      case Nil => List()
      case List(i) => List(i)
      case lst => lst.splitAt(lst.size / 2) match {
        case (l, r) =>
          merge(mSort(l), mSort(r), Nil).reverse } } }

  def timeGrowth[A](l: List[A], fn: List[A] => List[A]): List[(Int,Long)] = {
    val intervals = (0 to 50).map(_ * l.size / 50).toList
    intervals map { i =>
      val sub = l.take(i)
      val elapsed = time(sub,fn)
      println(s"$i, $elapsed")
      (i,elapsed) } }

  def time[A](l: List[A], fn: List[A] => List[A]): Long = {
    System.gc()
    val start = System.nanoTime
    fn(l)
    (System.nanoTime - start) / 1000 /*to millis*/ }

  def isLinear(growth: List[(Int,Long)]): Boolean = ???
  def isNLogN(growth: List[(Int,Long)]): Boolean = ???
  def isQuadratic(growth: List[(Int,Long)]): Boolean = ???

}
