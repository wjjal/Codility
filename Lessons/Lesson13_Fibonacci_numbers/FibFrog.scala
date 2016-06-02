//The Fibonacci sequence is defined using the following recursive formula:
//    F(0) = 0
//    F(1) = 1
//    F(M) = F(M - 1) + F(M - 2) if M >= 2
//A small frog wants to get to the other side of a river. The frog is initially located at one bank of the river (position −1) and wants to get to the other bank (position N). The frog can jump over any distance F(K), where F(K) is the K-th Fibonacci number. Luckily, there are many leaves on the river, and the frog can jump between the leaves, but only in the direction of the bank at position N.
//The leaves on the river are represented in a zero-indexed array A consisting of N integers. Consecutive elements of array A represent consecutive positions from 0 to N − 1 on the river. Array A contains only 0s and/or 1s:
//0 represents a position without a leaf;
//1 represents a position containing a leaf.
//The goal is to count the minimum number of jumps in which the frog can get to the other side of the river (from position −1 to position N). The frog can jump between positions −1 and N (the banks of the river) and every position containing a leaf.
//For example, consider array A such that:
//    A[0] = 0
//    A[1] = 0
//    A[2] = 0
//    A[3] = 1
//    A[4] = 1
//    A[5] = 0
//    A[6] = 1
//    A[7] = 0
//    A[8] = 0
//    A[9] = 0
//    A[10] = 0
//The frog can make three jumps of length F(5) = 5, F(3) = 2 and F(5) = 5.
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given a zero-indexed array A consisting of N integers, returns the minimum number of jumps by which the frog can get to the other side of the river. If the frog cannot reach the other side of the river, the function should return −1.
//For example, given:
//    A[0] = 0
//    A[1] = 0
//    A[2] = 0
//    A[3] = 1
//    A[4] = 1
//    A[5] = 0
//    A[6] = 1
//    A[7] = 0
//    A[8] = 0
//    A[9] = 0
//    A[10] = 0
//the function should return 3, as explained above.
//Assume that:
//N is an integer within the range [0..100,000];
//each element of array A is an integer that can have one of the following values: 0, 1.
//Complexity:
//expected worst-case time complexity is O(N*log(N));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Lessons.Lesson13_Fibonacci_numbers

import scala.collection.mutable.MutableList

object FibFrog {
  def solution(A: Array[Int]): Int = {
    val N = A.length
    var fibs = fibonacciDynamic(N + 1)
    var status = new MutableList[Int]
    for (length <- fibs) {
      var pos = -1 + length
      if (pos == N)
        return 1
      if (A(pos) == 1)
        status += pos
    }
    var flags = new Array[Boolean](N + 2)
    bfSearch(A, fibs, status, flags, 1, N)
  }

  def bfSearch(leaf: Array[Int], fibs: Array[Int], status: MutableList[Int], flag: Array[Boolean], moves: Int, N: Int): Int = {
    if (status.length == 0)
      return -1
    var new_status = new MutableList[Int]
    var result = -1
    for (k <- status) {
      if (!flag(k)) {
        for (length <- fibs) {
          var pos = k + length
          if (pos == N)
            return moves + 1
          if (pos < N + 1 && !flag(pos)&& leaf(pos) == 1) {
            new_status += pos
          }
        }
        flag(k) = true
      }
    }
    result = bfSearch(leaf, fibs, new_status, flag, moves + 1, N)
    result
  }

  def fibonacciDynamic(N: Int): Array[Int] = {
    var fibs = new Array[Int](N + 2)
    fibs(0) = 0
    fibs(1) = 1
    for (i <- 2 until N + 2) {
      var tmp = fibs(i - 1) + fibs(i - 2)
      if (tmp > N) {
        return fibs.filter(_ != 0)
      } else
        fibs(i) = tmp
    }
    return fibs.filter(_ != 0)
  }
}