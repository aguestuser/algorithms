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

  def depth(tr: Tree): Int = tr match {
    case Empty() => 0
    case Leaf(_) => 1
    case Branch(_,l,r) => 1 + depth(l) max depth(r) }

  def map(tr: Tree)(fn: Int => Int): Tree = tr match {
    case Empty() => Empty()
    case Leaf(v) => Leaf(fn(v))
    case Branch(v,l,r) => Branch(fn(v), map(l)(fn), map(r)(fn)) }

  def fold(tr: Tree)(fn: Int => Int): Int = ???

  def isBalanced(tr: Tree): Boolean = tr match {
    case Empty() => false
    case Leaf(_) => true
    case Branch(_,l,r) => isBalanced(l) && isBalanced(r) }

}


