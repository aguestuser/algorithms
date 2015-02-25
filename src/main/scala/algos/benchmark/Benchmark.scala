package algos.benchmark

/**
 * Author: @aguestuser
 * Date: 2/25/15
 * License: GPLv2
 */

object Benchmark {

  def plotGrowth[A](l: List[A], fn: List[A] => List[A]): List[(Int,Long)] = {
    val intervals = (0 to 50).map(_ * l.size / 50).toList
    intervals map { i =>
      val sub = l.take(i)
      val elapsed = time(sub,fn)
      println(s"$i, $elapsed")
      (i,elapsed) } }

  def timeGrowth[A](l: List[A], fn: List[A] => List[A]): List[Double] = {
    val intervals = (0 to 50).map(_ * l.size / 50).toList
    intervals map { i =>
      val sub = l.take(i)
      time(sub,fn).toDouble } }

  def time[A](l: List[A], fn: List[A] => List[A]): Long = {
    System.gc()
    val start = System.nanoTime
    fn(l)
    (System.nanoTime - start) / 1000 /*to millis*/ }

  def delta(a: Double, b: Double): Double = Math.abs((a-b)/a)

  def isLinear(growth: List[Double]): Boolean = growth match {
    case (fst :: snd :: tail) =>
      ((true, delta(fst,snd),snd) /: tail)((acc,dub) => acc match {
        case (res,lastDelt,lastLong) =>
          val newDelt = delta(lastDelt,delta(lastLong,dub))
          val newRes = newDelt < .1
          (newRes,newDelt,dub) })._1 }

  def isQuadratic(growth: List[Double]): Boolean = isLinear(growth.map(Math.sqrt))

//  def isNLogN(growth: List[Long]): Boolean = ???

}
