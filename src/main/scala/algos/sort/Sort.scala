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
    def sort(l: Int, r: Int): Unit = {
      if (l < r) {
        val p = choosePivot(l,r)
        val pp = partition(as,p,l,r)
        sort(l, pp - 1)
        sort(pp + 1, r) } }
    sort(0,as.size-1); as }

  //imperative random select
  def rSelectA[A](as: ArrayBuffer[A], k: Int)(implicit o: Ordering[A]): A = {
    @tailrec
    def select(l: Int, r: Int, ki: Int): A = {
      if (l > r) throw new Exception("Can't access an array of negative length")
      else if (l == r) as(l)
      else {
        val p = choosePivot(l,r)
        val pp = partition(as,p,l,r)
        if (ki == pp) as(pp) else if (ki < pp) select(l,pp-1,ki) else select(pp+1,r,ki) } }
    select(0,as.size-1,k-1) }


  // imperative partitioning helpers
  def partition[A](as: ArrayBuffer[A], p: Int, l: Int, r: Int)(implicit o: Ordering[A]): Int = {
    val piv = as(p); swap(as,p,l)
    @tailrec
    def partitionOne(i: Int, ll: Int, rr: Int): Int = {
      if (ll > rr) { swap(as,i-1,l); i-1 }
      else {
        val newI = if (o.lt(as(ll),piv)){ swap(as,ll, i); i+1 } else i
        partitionOne(newI,ll+1,rr) } }
    partitionOne(l+1,l+1,r) }

  def choosePivot[A](l: Int, r: Int): Int = l + (random * (abs(l-r)+1)).toInt
  def swap[A](as: ArrayBuffer[A], i: Int, j: Int) { val t = as(i); as(i) = as(j); as(j) = t }


  //functional quicksort
  def qSortL[A](as: List[A])(implicit o: Ordering[A]): List[A] = as match {
    case Nil => List()
    case List(a) => as
    case _ =>
      val p = choosePivot(as)
      val (lt,eq,gt) = partition(as, p)
      qSortL(lt); qSortL(gt) }

  // functional random select
  def rSelect[A](as: List[A], k: Int)(implicit o: Ordering[A]): A = {
    @tailrec
    def select(aas: List[A], ki: Int): A = aas match {
      case Nil => throw new Exception("Can't select from an empty list")
      case List(i) => i
      case _ =>
        val p = choosePivot(aas)
        val (lt,_,gt) = partition(aas,p)
        if (lt.size == ki) p else if (lt.size > ki) select(lt,ki) else select(gt,ki-lt.size-1) }
    select(as,k-1) }

  // functional partition helpers
  def choosePivot[A](as: List[A]) = as((random * as.size).toInt)
  def partition[A](as: List[A], p: A)(implicit o: Ordering[A]): (List[A],List[A],List[A]) =
    (as.filter(o.lt(_,p)), as.filter(o.eq(_,p)), as.filter(o.gt(_,p)))

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