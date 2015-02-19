package algos.sort

/**
 * Author: @aguestuser
 * Date: 2/18/15
 * License: GPLv2
 */

object CountInversions {

  type Ints = Vector[Int]
  
  def countInversions(ints: Ints): Long = sortCount(ints,0)._2
  
  def sortCount(ints: Ints, count: Long): (Ints,Long) = ints match {

    case Vector() => (Vector(), count)
    case Vector(n) => (Vector(n), count)
    case _ =>
      val (l,r) = ( ints.slice(0,ints.size/2), ints.slice(ints.size/2,ints.size) )

      val (lSort, lCount) = sortCount(l,0)
      val (rSort, rCount) = sortCount(r,0)
      val (mSort, mCount) = mergeCount(lSort,rSort)

      (mSort, lCount + rCount + mCount) }

  def mergeCount(left: Ints, right: Ints): (Ints,Long) = {

    @annotation.tailrec
    def loop(l: Ints, r: Ints, acc: Ints, count: Long): (Ints, Ints, Ints, Long) =
      (l,r) match {
        case (Vector(), Vector()) => (Vector(),Vector(),acc,count)
        case (Vector(), r_) => (Vector(),Vector(),acc ++ r, count)
        case (l_, Vector()) => (Vector(),Vector(),acc ++ l, count)
        case (l_,r_) =>
          if (l.head <= r.head) loop(l.tail, r, acc :+ l.head, count)
          else loop(l, r.tail, acc :+ r.head, count + l.size) }

    loop(left,right,Vector[Int](),0) match { case(_,_,a,c) => (a,c) } }
  
}
