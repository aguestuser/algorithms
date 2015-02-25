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

  def dutchFlagify[A](l: List[A])(implicit o: Ordering[A]): List[A] = l match {
    case Nil => List()
    case _ =>
      val pivot = l(l.size/2)
      ((List[A](),List[A](),List[A]()) /: l)((acc,i) =>
        acc match { case(lt,eq,gt) =>
          if (o.compare(i,pivot) < 0) (i::lt,eq,gt)
          else if (o.compare(i,pivot) < 0) (lt,eq,i::gt)
          else (lt,i::eq,gt)}) match { case(lt,gt,eq) =>
        lt ::: gt ::: eq } }




}
