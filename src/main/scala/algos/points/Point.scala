package algos.points

import algos.sort.Sort._
import scala.annotation.tailrec

/**
 * Author: @aguestuser
 * Date: 2/20/15
 * License: GPLv2
 */


case class Point(x: Int, y:Int)

object XOrdering extends Ordering[Point] {
  def compare(a: Point, b: Point) = a.x compare b.x }

object YOrdering extends Ordering[Point] {
  def compare(a: Point, b: Point) = a.y compare b.y }

object Point {

  type P = Point
  type Ps = List[Point]

  def distance(ps: (P,P)): Double = ps match {
    case (p1,p2) =>
      val (dx,dy) = (p1.x - p2.x, p1.y - p2.y)
      Math.sqrt(dx*dx - dy*dy) }

  def closer(ps:(P,P), qs:(P,P)): ((P,P),Double) = {
    val (distP,distQ) = (distance(ps), distance(qs))
    if (distP < distQ) (ps,distP) else (qs,distQ) }

  def closestPair(ps: Set[P]): (P,P) = closestPair(ps.toList)
  def closestPair(ps: Ps): (P,P) = closestPair(mSort(ps)(XOrdering), mSort(ps)(YOrdering))
  def closestPair(pX: Ps, pY: Ps) : (P,P) = {

    // TODO add base case!
    val pivot = pX(pX.size/2).x
    val (lX,rX) = pX splitAt pivot
    val (lY,rY) = split(pY,pivot) // linear!

    val (closestL,closestR) = (closestPair(lX,lY),closestPair(rX,rY))
    val (closest,closestDist) = closer(closestL,closestR)

    val closestS = closestSplitPair(pX,pY,closestDist)
    closer(closest,closestS)._1 }

  private def split(ps: Ps, pivot: Int): ((Ps,Ps)) = {
    @tailrec
    def loop(ps: Ps, splitPs:(Ps,Ps)): (Ps,(Ps,Ps)) = ps match {
      case Nil => (ps,splitPs)
      case _ =>
        splitPs match { case(lY,rY) =>
          if (ps.head.x <= pivot) loop(ps.tail, (lY :+ ps.head, rY))
          else loop(ps.tail, (lY, rY :+ ps.head)) } }
    loop(ps,(List(),List())) match { case (_,(lY,rY)) => (lY,rY) } }

  private def closestSplitPair(pX: Ps, pY: Ps, dist: Double): (P,P) = {
    val x_ = pX((pX.size/2)-1).x
    val ps = pY takeWhile { p => (p.x <= x_ + dist) || (p.x >= x_ + dist) } // linear time
    ((ps(0),ps(1)) /: ps)( (winner,p) =>
      closer(winner, (winner /: ps.tail.take(8))((cw,p) => // constant time b/c inner loop only iterates 8x
        closer(cw,(ps.head,p))._1))._1) }

}
