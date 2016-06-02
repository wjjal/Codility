//In this problem we consider binary trees, represented by pointer data-structures. A pointer is called a binary tree if:
//it is an empty pointer (it is then called an empty tree); or
//it points to a structure (called a node) that contains a value and two pointers that are binary trees (called the left subtree and the right subtree).
//A figure below shows a tree consisting of six nodes.
//A path in a binary tree is a sequence of nodes one can traverse by following the pointers. More formally, a path is a sequence of nodes P[0], P[1], ..., P[K], such that node P[L] contains a pointer pointing to P[L + 1], for 0 ≤ L < K. K is called the length of such a path.
//The height of a binary tree is defined as the length of the longest possible path in the tree. In particular, a tree consisting only of just one node has height 0 and the height of an empty tree is undefined.
//For example, consider the following tree:
//
//Subtrees of nodes D, E and F are empty trees. Sequence A, B, E is a path of length 2. Sequence C, F is a path of length 1. Sequence E, B, D is not a valid path. The height of this tree is 2.
//Assume that the following declarations are given:
//class Tree(var x: Int, var l: Tree, var r: Tree)
//Write a function:
//object Solution { def solution(T: Tree): Int }
//that, given a non-empty binary tree T consisting of N nodes, returns its height.
//For example, given tree T shown in the example above, the function should return 2.
//Assume that:
//N is an integer within the range [1..1,000].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N).

package Lessons.Lesson99_Future_training

import scala.math._

object TreeHeight {
  def solution(T: Tree): Int = {
    getHeight(T) - 1
  }

  def getHeight(T: Tree): Int = {
    if (T == null)
      return 0
    max(getHeight(T.l), getHeight(T.r)) + 1
  }
}