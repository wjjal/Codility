//A non-empty zero-indexed array A consisting of N different integers is given. We are looking for the longest possible sequence built from elements of A, A[B[0]], A[B[1]], ..., A[B[K]], satisfying the following conditions:
//The sequence must be decreasing; that is, A[B[0]] > A[B[1]] > ... > A[B[K]].
//For any two consecutive elements of the sequence, A[B[I]] and A[B[I+1]], all the elements of A between them must be smaller than them; that is, for any J = MIN(B[I], B[I+1]) + 1, ..., MAX(B[I], B[I+1]) − 1, we have A[J] < A[B[I+1]].
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given a zero-indexed array A containing N different integers, computes the maximum length of a sequence satisfying the above conditions.
//For example, for the following array A:
//  A[0]  =  9   A[1]  = 10   A[2]  =  2
//  A[3]  = -1   A[4]  =  3   A[5]  = -5
//  A[6]  =  0   A[7]  = -3   A[8]  =  1
//  A[9]  = 12   A[10] =  5   A[11] =  8
//  A[12] = -2   A[13] =  6   A[14] =  4
//the function should return 6.
//A sequence of length 6 satisfying the given conditions can be as follows:
//  A[9] = 12    A[1] = 10    A[4] =  3
//  A[8] =  1    A[6] =  0    A[7] = -3
//Assume that:
//the elements of A are all distinct;
//N is an integer within the range [1..100,000];
//each element of array A is an integer within the range [−1,000,000,000..1,000,000,000].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges_2012

//笛卡尔树
import scala.math._

object Upsilon2012 {
  def main(args: Array[String]) {
    val A = Array(9, 10, 2, -1, 3, -5, 0, -3, 1, 12, 5, 8, -2, 6, 4)
    println(solution(A))
  }

  def solution(A: Array[Int]): Int = {
    val N = A.length
    if (N == 0)
      return 0
    var root = new treeNode(A(0))
    var prev = root
    for (i <- 1 until N) {
      var cur = new treeNode(A(i))
      var break = false
      while (!break && prev.data <= cur.data) {
        //Finding a node with value greater than current node
        //Update the height information of nodes on the finding path
        //These nodes will NEVER be accessed again!
        //AND their height information is final.
        if (prev.left != null)
          prev.height = max(prev.left.height + 1, prev.height)
        if (prev.right != null)
          prev.height = max(prev.right.height + 1, prev.height)
        //Cannot find the node with greater value
        //Current node will be the new root node
        if (prev.parent == null) {
          cur.height = prev.height + 1
          cur.left = prev
          prev.parent = cur
          root = cur
          break = true
        }
        if (!break)
          prev = prev.parent
      }
      //Find the node, to say x, with greater value
      //x.right will be current node's left son
      //And current node will be the x's new right son
      if (prev.data > cur.data) {
        cur.left = prev.right
        cur.parent = prev
        if (cur.left != null) {
          cur.left.parent = cur
          cur.height = cur.left.height + 1
        }
        prev.right = cur
      }
      prev = cur
    }
    //Some nodes, on the path from last processed node to root,
    //might have out-of-date height information. Update them.
    while (prev.parent != null) {
      prev = prev.parent
      prev.height = max(prev.height, prev.right.height + 1)
    }
    root.height + 1
  }

  class treeNode(d: Int) {
    var data = d
    var height = 0
    var parent: treeNode = null
    var left: treeNode = null
    var right: treeNode = null
  }
}