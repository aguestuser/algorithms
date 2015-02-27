package algos.sort

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */


object Sort {

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

  def iqSort[A](as: ArrayBuffer[A])(implicit o: Ordering[A]): ArrayBuffer[A] = {

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

    def choosePivot(l: Int, r: Int): Int = (l + r) / 2
    def swap(i: Int, j: Int) { val t = as(i); as(i) = as(j); as(j) = t }

    sort(0,as.size-1) }

  def fqSort[A](as: List[A])(implicit o: Ordering[A]): List[A] = as match {
    case Nil => List()
    case List(a) => as
    case _ =>
      val p = choosePivot(as)
      List.concat(
        fqSort(as.filter(o.compare(_,p) < 0)),
        as.filter(o.compare(_,p) == 0),
        fqSort(as.filter(o.compare(_,p) > 0))) }


  def choosePivot[A](as: List[A]): A = as(as.size/2)


  def dutchFlagify[A](l: List[A])(implicit o: Ordering[A]): List[A] = l match {
    case Nil => List()
    case _ =>
      val pivot = l(l.size/2)
      ((List[A](),List[A](),List[A]()) /: l)((acc,i) =>
        acc match { case(lt,eq,gt) =>
          if (o.compare(i,pivot) < 0) (i::lt,eq,gt)
          else if (o.compare(i,pivot) < 0) (lt,eq,i::gt)
          else (lt,i::eq,gt)}) match { case(lt,gt,eq) =>
        lt ::: gt ::: eq } }




}
