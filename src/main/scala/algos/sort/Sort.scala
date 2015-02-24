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
      case (Nil, _) => acc ++ r
      case (_, Nil) => acc ++ l
      case (_,_) =>
        if (o.compare(l.head, r.head) <= 0) merge(l.tail, r, acc :+ l.head)
        else merge(l, r.tail, acc :+ r.head) }

    as match {
      case Nil => List()
      case List(i) => List(i)
      case lst => lst.splitAt(lst.size / 2) match {
        case (l, r) =>
          merge(mSort(l), mSort(r), Nil) } } }
}
