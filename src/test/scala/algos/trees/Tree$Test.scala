package algos.trees

import algos.trees.Tree._

import org.specs2.mutable.Specification

/**
 * Author: @aguestuser
 * Date: 2/17/15
 * License: GPLv2
 */

class Tree$Test extends Specification {

  import algos.trees.SampleTrees._

  "Tree module" should {

    "count the nodes in a tree" in {
      count(simpleTree) === 7
      count(bsTree) === 10
    }

    "sum the nodes in a tree" in {
      sum(simpleTree) === 28
      sum(bsTree) === 55
    }

    "find the max value in a tree" in {
      maximum(simpleTree) === 7
      maximum(bsTree) === 10
    }

    "find the depth of a tree" in {
      depth(simpleTree) === 3
      depth(bsTree) === 4
      depth(unbalancedTree) === 4
    }

    " map a function over a tree" in {

      Tree.map { simpleTree } { _ * 2 } ===
        Branch(2,
          Branch(4,
            Leaf(6),
            Leaf(8)),
          Branch(10,
            Leaf(12),
            Leaf(14)))

      Tree.map { bsTree } { _ * 2 } ===
        Branch(20,
          Branch(8,
            Branch(6,
              Leaf(2),
              Leaf(4)),
            Branch(12,
              Leaf(10),
              Empty())),
          Branch(16,
            Leaf(14),
            Leaf(18)))
    }

    "discover whether a tree is balanced" in {

      isBalanced(simpleTree) === true
      isBalanced(bsTree) === true
      isBalanced(unbalancedTree) === false
    }

    "discover whether a tree is complete" in {

      isFull(simpleTree) === true
      isFull(bsTree) === false
      isFull(unbalancedTree) === false
    }
  }
}

object SampleTrees {

  /*
  *           1
  *         /   \
  *        2    5
  *       / \  / \
  *      3  4 6  7
  * */

  lazy val simpleTree =
    Branch(1,
      Branch(2,
        Leaf(3),
        Leaf(4)),
      Branch(5,
        Leaf(6),
        Leaf(7)))

  /*
  *          10
  *       /      \
  *      4       8
  *    /   \    /  \
  *   3     6  7   9
  *  / \   /
  * 1  2  5
  *
  *
  * */
  lazy val bsTree =
    Branch(10,
      Branch(4,
        Branch(3,
          Leaf(1),
          Leaf(2)),
        Branch(6,
          Leaf(5),
          Empty())),
      Branch(8,
        Leaf(7),
        Leaf(9)))

  /*
  *         1
  *       /   \
  *      2    6
  *     / \
  *    3  4
  *   /
  *  5
  *
  * */

  lazy val unbalancedTree =
    Branch(1,
    Branch(2,
      Branch(3,
        Leaf(5),
        Empty()),
      Leaf(4)),
    Leaf(6))

 }