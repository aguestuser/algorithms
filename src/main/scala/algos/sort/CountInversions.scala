package algos.sort

/**
 * Author: @aguestuser
 * Date: 2/18/15
 * License: GPLv2
 */

object CountInversions {

  def countInversions(ints: Array[Int]) = sortCount(ints,0)._2
  
  def sortCount(ints: Array[Int], count: Int): (Array[Int],Int) = ints match {

    case Array() => (Array(), count)
    case Array(n) => (Array(n), count)
    case _ =>
      val (l,r) = ( ints.slice(0,ints.size/2), ints.slice(ints.size/2,ints.size) )
      val (lSort, lCount) = sortCount(l,0)
      val (rSort, rCount) = sortCount(r,0)
      val (mSort, mCount) = mergeCount(lSort,rSort,0)

      (mSort, lCount + rCount + mCount) }

  def mergeCount(left: Array[Int], right: Array[Int], count: Int): (Array[Int],Int) =

    (left,right) match {
      case (Array(), r) => (r, count)
      case (l, Array()) => (l,count)
      case (l,r) =>
        if (l.head <= r.head) {
          val (mergedTail, tailCount) = mergeCount(l.tail, r, count)
          (l.head +: mergedTail, count + tailCount) }
        else {
          val (mergedTail, tailCount) = mergeCount(l, r.tail, count)
          (r.head +: mergedTail, count + l.size + tailCount) } }


}
