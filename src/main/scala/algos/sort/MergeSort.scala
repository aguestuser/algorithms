package algos.sort

import scala.annotation.tailrec

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */

object MergeSort {

//  type Ints = Vector[Int]
//  type IntPair = (Int,Int)
//  type IntPairs = Vector[IntPair]

  def sort[A](as: Vector[A])(implicit o: Ordering[A]) : Vector[A] = as match { // log n steps
    case Vector() => Vector()
    case Vector(i) => Vector(i)
    case v =>
      merge(sort(v.slice(0, v.size/2)), sort(v.slice(v.size/2, v.size)))(o) }

  private def merge[A](left: Vector[A], right: Vector[A])(o: Ordering[A]) : Vector[A] = {

    @tailrec
    def loop(lft: Vector[A], rt: Vector[A], acc: Vector[A]): (Vector[A], Vector[A], Vector[A]) =
      (lft,rt) match {
        case (Vector(),Vector()) => (Vector(), Vector(),acc)
        case (Vector(), r) => (Vector(), Vector(), acc ++ r)
        case (l, Vector()) => (Vector(), Vector(), acc ++ l)
        case (l,r) =>
          if (o.compare(l.head,r.head) <= 0) loop(l.tail, r, acc :+ l.head )
          else loop(l, r.tail, acc :+ r.head) }

    loop(left,right,Vector())._3 }

}
