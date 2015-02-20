package algos.trees

/**
 * Author: @aguestuser
 * Date: 2/17/15
 * License: GPLv2
 */

sealed trait Tree
case class Branch(v: Int, l: Tree, r: Tree) extends Tree
case class Leaf(v: Int) extends Tree
case class Empty() extends Tree

object Tree {

  def count(tr: Tree): Int = tr match {
    case Empty() => 0
    case Leaf(v) => 1
    case Branch(_,l,r) => 1 + count(l) + count(r) }

  def sum(tr: Tree): Int = tr match {
    case Empty() => 0
    case Leaf(v) => v
    case Branch(v,l,r) => v + sum(l) + sum(r) }

  def maximum(tr: Tree): Int = tr match {
    case Empty() => Int.MinValue
    case Leaf(v) => v
    case Branch(v,l,r) => v max maximum(l) max maximum(r) }

  def height(tr: Tree): Int = tr match {
    case Empty() => 0
    case Leaf(_) => 1
    case Branch(_,l,r) => 1 + height(l) max height(r) }

  def map(tr: Tree)(fn: Int => Int): Tree = tr match {
    case Empty() => Empty()
    case Leaf(v) => Leaf(fn(v))
    case Branch(v,l,r) => Branch(fn(v), map(l)(fn), map(r)(fn)) }

  def fold(tr: Tree)(fn: Int => Int): Int = ???

  def isBalanced(tree: Tree): Boolean = checkBalanced(tree)._1

  def checkBalanced(t: Tree): (/*isComplete*/Boolean,/*height*/Int) = t match {
    case Empty() => (true, 0)
    case Leaf(_) => (true, 1)
    case Branch(_, l, r) =>
      val (lBal, lHt) = checkBalanced(l)
      val (rBal, rHt) = checkBalanced(r)
      if (!lBal || !rBal) (false, 0)
      else (Math.abs(lHt - rHt) <= 1, 1 + (lHt max rHt) }

  def isComplete(tree: Tree): Boolean = checkComplete(tree)._1
  def largestComplete(tree: Tree): Int = checkComplete(tree)._2

  def checkComplete(t: Tree): (/*isComplete*/Boolean,/*height*/Int) = t match {
    case Empty() => (true,0)
    case Leaf(_) => (true,1)
    case Branch(_,l,r) =>
      val (lCompl, lHt) = checkComplete(l)
      val (rCompl, rHt) = checkComplete(r)
      (lCompl,rCompl) match {
        case(false,false) => (false,0)
        case(true,false) => (false,lHt)
        case(false,true) => (false,rHt)
        case(true,true) => ( Set(0,1).contains(lHt - rHt), 1 + (lHt max rHt) ) } }

  def isFull(tr: Tree): Boolean = tr match {
    case Empty() => false
    case Leaf(_) => true
    case Branch(_,l,r) => isFull(l) && isFull(r) }

  def isKBalanced(tree: Tree, k: Int): Boolean = checkKBalanced(tree,k)._1
  def largestUnKBalancedParent(tree: Tree, k: Int): Option[Tree] = ???

  def checkKBalanced(tree: Tree, k: Int): (Boolean,Int,Tree) = tree match {
    case Empty() => (true,0,Empty())
    case Leaf(x) => (true,1,Leaf(x))
    case Branch(_,l,r) =>
      val (lKBal, lHt, _) = checkKBalanced(l,k)
      val (rKBal, rHt, _) = checkKBalanced(r,k)
      (lKBal,rKBal) match {
        case(false,false) => (false,0,Empty())
        case(true,false) => (false,lHt,l)
        case(false,true) => (false,rHt,r)
        case(true,true) => ( Math.abs(lHt - rHt) <= k, 1 + (lHt max rHt), tree ) } }
  //TODO test this!!!
  //TODO analyze time complexity (how to get early return in Scala???)


  def isPerfect(tr: Tree): Boolean = ???


}


