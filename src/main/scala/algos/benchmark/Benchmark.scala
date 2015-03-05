package algos.benchmark

import scala.collection.SeqLike
import scala.math.random


/**
 * Author: @aguestuser
 * Date: 2/25/15
 * License: GPLv2
 */

object Benchmark {

  def time[A, T[B] <: SeqLike[B, T[B]], U[B] <: SeqLike[B, U[B]]]
  (l: T[A], fn: T[A] => U[A]): Long = {
    System.gc()
    val start = System.nanoTime
    fn(l)
    (System.nanoTime - start) / 1000 /*to millis*/ }

  def plotGrowth[A, T[B] <: SeqLike[B, T[B]], U[B] <: SeqLike[B, U[B]]]
  (l: T[A], fn: T[A] => U[A]): Boolean = {
    val intervals = (0 to 50).map(_ * l.size / 50)
    intervals map { i =>
      val sub = l.take(i)
      val elapsed = time(sub,fn)
      println(s"$i, $elapsed") }
    true }

  def plotGrowthWithK[A, T[B] <: SeqLike[B, T[B]]]
  (l: T[A], fn: (T[A],Int) => A): Boolean = {
    val intervals = (0 to 50).map(_ * l.size / 50)
    intervals map { i =>
      val sub = l.take(i)
      val k = (random*i).toInt
      System.gc()
      val start = System.nanoTime
      fn(l,k)
      val elapsed = (System.nanoTime - start) / 1000
      println(s"$i, $elapsed") }
    true }



  def delta(a: Double, b: Double): Double = Math.abs((a-b)/a)

  def isLinearNaive(growth: List[Double]): Boolean = growth match {
    case (fst :: snd :: tail) =>
      ((true, delta(fst,snd),snd) /: tail)((acc,dub) => acc match {
        case (res,lastDelt,lastLong) =>
          val newDelt = delta(lastDelt,delta(lastLong,dub))
          val newRes = newDelt < .1
          (newRes,newDelt,dub) })._1
    case _=> throw new Exception("whoops!")}

  def isQuadratic(growth: List[Double]): Boolean = isLinearNaive(growth.map(Math.sqrt))

//  def isNLogN(growth: List[Long]): Boolean = ???


}
