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
      count(perfectTree) === 7
      count(binarySearchTree) === 10
    }

    "sum the nodes in a tree" in {
      sum(perfectTree) === 28
      sum(binarySearchTree) === 55
    }

    "find the max value in a tree" in {
      maximum(perfectTree) === 7
      maximum(binarySearchTree) === 10
    }

    "find the depth of a tree" in {
      height(perfectTree) === 3
      height(binarySearchTree) === 4
      height(unbalancedIncompleteTree) === 4
    }

    " map a function over a tree" in {

      Tree.map { perfectTree } { _ * 2 } ===
        Branch(2,
          Branch(4,
            Leaf(6),
            Leaf(8)),
          Branch(10,
            Leaf(12),
            Leaf(14)))

      Tree.map { binarySearchTree } { _ * 2 } ===
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

      isBalanced(perfectTree) === true
      isBalanced(binarySearchTree) === true
      isBalanced(unbalancedIncompleteTree) === false
      isBalanced(balancedIncompleteTree) === true
    }

    "discover whether a tree is full" in {

      isFull(perfectTree) === true
      isFull(binarySearchTree) === false
      isFull(unbalancedIncompleteTree) === false
    }

    "discover whether a tree is complete" in {

      isComplete(perfectTree) === true
      isComplete(unbalancedIncompleteTree) === false
      isComplete(balancedIncompleteTree) === false
      isComplete(completeImperfectTree) === true
    }

    "find the height of the largest complete subtree of a tree" in {

      largestComplete(perfectTree) === 3
      largestComplete(balancedIncompleteTree) === 4
      largestComplete(unbalancedIncompleteTree) === 1
      largestComplete(balancedIncompleteTree) === 4
      largestComplete(completeImperfectTree) === 3

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

  lazy val perfectTree =
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
  lazy val binarySearchTree =
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
  *     /
  *    3
  *   /
  *  5
  *
  * */

  lazy val unbalancedIncompleteTree =
    Branch(1,
    Branch(2,
      Branch(3,
        Leaf(5),
        Empty()),
      Empty()),
    Leaf(6))
  
  /*
  * 
  *               1
  *            /     \
  *           2       3
  *         /   \    / \
  *       4     5   6  7
  *      / \   / \
  *     8  9  10 11
  *    / \
  *  12  13
  * 
  * */
  
  lazy val balancedIncompleteTree =
    Branch(1,
      Branch(2,
        Branch(4,
          Leaf(8),
          Leaf(9)),
        Branch(5,
          Leaf(10),
          Leaf(11))),
      Branch(3,
        Leaf(6),
        Leaf(7)))

  /*
  *
  *              1
  *            /   \
  *           2     3
  *          / \
  *         4  5
  *
  * */

  lazy val completeImperfectTree =
    Branch(1,
      Branch(2,
        Leaf(4),
        Leaf(5)),
      Leaf(3))

 }