package algos.sort

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */

object MergeSort {

  def sort(ints: Array[Int]) : Array[Int] = ints match { // log n steps
    case Array() => Array()
    case Array(i) => Array(i)
    case arr =>
      val piv = arr.size/2
      merge(sort(arr.slice(0, piv)), sort(arr.slice(piv, arr.size))) // this is where the dividing happens
  }

  private def merge(lft: Array[Int], rt: Array[Int]) : Array[Int] = (lft,rt) match { // n steps (will get called from sort log n times)
    case (Array(), r) => r
    case (l, Array()) => l
    case (l,r) if l.head <= r.head => l.head +: merge(l.tail, r)
    case (l,r) if r.head < l.head => r.head +: merge(l, r.tail)
  }
}
