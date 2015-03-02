package algos.sort

import java.lang.Math.{abs, random}

import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.mutable.ArrayBuffer

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */

object Sort {

  // merge sort on lists
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

  //merge sort on vectors
  def mSortV[A](as: Vector[A])(implicit o: Ordering[A]): Vector[A] = {

    @tailrec
    def merge(l: Vector[A], r: Vector[A], acc: Vector[A]): Vector[A] = (l, r) match {
      case (Vector(), _) => acc ++ r
      case (_, Vector()) =>  acc ++ l
      case (_,_) =>
        if (o.compare(l.head, r.head) <= 0) merge(l.tail, r, acc :+ l.head)
        else merge(l, r.tail, acc :+ r.head) }

    as match {
      case Vector() => Vector()
      case Vector(i) => Vector[A](i)
      case lst => lst.splitAt(lst.size / 2) match {
        case (l, r) =>
          merge(mSortV(l), mSortV(r), Vector[A]()) } } }

  //imperative quicksort
  def qSort[A](as: ArrayBuffer[A])(implicit o: Ordering[A]): ArrayBuffer[A] = {

    def sort(l: Int, r: Int): ArrayBuffer[A] = {
      if (l >= r) as
      else {
        val p = choosePivot(l,r)
        val pp = partition(p,l,r)
        sort(l, pp - 1); sort(pp + 1, r) } }

    def partition(p: Int, l: Int, r: Int): Int = {
      val piv = as(p); swap(p,l)
      @tailrec
      def partitionOne(i: Int, ll: Int, rr: Int): Int = {
        if (ll > rr) { swap(i-1,l); i-1 }
        else {
          val newI = if (o.compare(as(ll),piv) < 0){swap(ll, i); i+1} else i
          partitionOne(newI,ll+1,rr) } }
      partitionOne(l+1,l+1,r) }

    def choosePivot(l: Int, r: Int): Int = l + (random * (abs(l-r)+1)).toInt
    def swap(i: Int, j: Int) { val t = as(i); as(i) = as(j); as(j) = t }

    sort(0,as.size-1) }

  //functional quicksort
  def qSortL[A](as: List[A])(implicit o: Ordering[A]): List[A] = as match {
    case Nil => List()
    case List(a) => as
    case _ =>
      val p = as((random * as.size).toInt)
      List.concat(
        qSortL(as.filter(o.compare(_,p) < 0)),
        as.filter(o.compare(_,p) == 0),
        qSortL(as.filter(o.compare(_,p) > 0))) }

  // property test
  def didSort[A, T[B] <: SeqLike[B, T[B]]](c1: T[A], c2: T[A])(implicit o: Ordering[A]): Boolean = {

    def sameSet(c1: T[A], c2: T[A]): Boolean = c1.toSet == c2.toSet
    def sameSize(c1: T[A], c2: T[A]): Boolean = c1.size == c2.size
    //    def correctBounds(c1: T[A], c2: T[A]): Boolean = c1.min == c2.head && c2.max == c2(c2.size-1)
    def ordered(c: T[A]): Boolean = {
      @tailrec
      def loop(lastRes: Boolean, lastA: A, as: T[A]): Boolean = as.size match {
        case 0 => lastRes
        case _ =>
          if (!lastRes) false
          else loop(lastRes && (o.compare(lastA,as.head) <= 0), as.head, as.tail) }
      loop(lastRes=true,c.head,c.tail) }

    sameSet(c1,c2) && sameSize(c1,c2) && /*correctBounds(c1,c2) &&*/ ordered(c2) }

}

//while loop version of partition for imperative quicksort
//    def partition(p: Int, l: Int, r: Int): Int = {
//      val piv = as(p); swap(p,l)
//      var i = l+1; var ll = l+1
//      while(ll <= r) {
//        if (o.compare(as(ll),piv) < 0){ swap(ll, i); i = i+1 }
//        ll = ll + 1}
//      swap(i-1,l); i-1 }

//tail recursive version:
//    def partition(p: Int, l: Int, r: Int): Int = {
//      val piv = as(p); swap(p,l)
//      @tailrec
//      def partitionOne(i: Int, ll: Int): Int = {
//        if (ll > r) { swap(i-1,l); i-1 }
//        else {
//          val newI = if (o.compare(as(ll),piv) < 0){ swap(ll, i); i+1 } else i
//          partitionOne(newI,ll+1) } }
//      partitionOne(l+1,l+1) }