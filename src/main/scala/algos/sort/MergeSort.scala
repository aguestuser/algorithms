package algos.sort

import scala.annotation.tailrec

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */

object MergeSort {

  type Ints = Vector[Int]

  def sort(ints: Ints) : Ints = ints match { // log n steps
    case Vector() => Vector()
    case Vector(i) => Vector(i)
    case arr =>
      merge(sort(arr.slice(0, arr.size/2)), sort(arr.slice(arr.size/2, arr.size))) }

  private def merge(left: Ints, right: Ints) : Ints = {

    @tailrec
    def loop(lft: Ints, rt: Ints, acc: Ints): (Ints, Ints, Ints) =
      (lft,rt) match {
        case (Vector(),Vector()) => (Vector(), Vector(),acc)
        case (Vector(), r) => (Vector(), Vector(), acc ++ r)
        case (l, Vector()) => (Vector(), Vector(), acc ++ l)
        case (l,r) =>
          if (l.head <= r.head) loop(l.tail, r, acc :+ l.head )
          else loop(l, r.tail, acc :+ r.head) }

    loop(left,right,Vector())._3 }

}
