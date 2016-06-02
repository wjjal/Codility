//Consider a zero-indexed array A of N integers. Indices of this array are integers from 0 to N−1. Take an index K. Index J is called an ascender of K if A[J] > A[K]. Note that if A[K] is a maximal value in the array A, then K has no ascenders.
//Ascender J of K is called the closest ascender of K if abs(K−J) is the smallest possible value (that is, if the distance between J and K is minimal). Note that K can have at most two closest ascenders: one smaller and one larger than K.
//For example, let us consider the following array A:
//  A[0] = 4     A[1]  = 3    A[2]  = 1
//  A[3] = 4     A[4]  = -1   A[5]  = 2
//  A[6] = 1     A[7]  = 5    A[8]  = 7
//If K = 3 then K has two ascenders: 7 and 8. Its closest ascender is 7 and distance between K and 7 equals abs(K−7) = 4.
//Write a function:
//object Solution { def solution(A: Array[Int]): Array[Int] }
//that, given a zero-indexed array A of N integers, returns a zero-indexed array R of N integers, such that (for K = 0,..., N−1):
//if K has the closest ascender J, then R[K] = abs(K−J); that is, R[K] is equal to the distance between J and K,
//if K has no ascenders then R[K] = 0.
//For example, given the following array A:
//  A[0] = 4     A[1]  = 3    A[2]  = 1
//  A[3] = 4     A[4]  = -1   A[5]  = 2
//  A[6] = 1     A[7]  = 5    A[8]  = 7
//the function should return the following array R:
//  R[0] = 7     R[1]  = 1    R[2]  = 1
//  R[3] = 4     R[4]  = 1    R[5]  = 2
//  R[6] = 1     R[7]  = 1    R[8]  = 0
//Array R should be returned as:
//a structure Results (in C), or
//a vector of integers (in C++), or
//a record Results (in Pascal), or
//an array of integers (in any other programming language).
//Assume that:
//N is an integer within the range [0..50,000];
//each element of array A is an integer within the range [−1,000,000,000..1,000,000,000].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Challenges.challenges_2012

import scala.math._
import scala.collection.mutable.Stack

object Pi2012 {
  def solution(A: Array[Int]): Array[Int] = {
    val N = A.length
    var re = new Array[Int](N)
    var left = new Stack[Int]()
    var right = new Stack[Int]()
    for (i <- 0 until N) {
      while (!left.isEmpty && A(left.top) <= A(i))
        left.pop
      if (left.isEmpty)
        re(i) = N
      else
        re(i) = i - left.top
      left.push(i)
    }
    for (i <- N - 1 to (0, -1)) {
      while (!right.isEmpty && A(right.top) <= A(i))
        right.pop
      if (!right.isEmpty)
        re(i) = Math.min(re(i), right.top - i)
      else if (re(i) == N)
        re(i) = 0
      right.push(i)
    }
    re
  }
}