//For a given array A of N integers and a sequence S of N integers from the set {−1, 1}, we define val(A, S) as follows:
//val(A, S) = |sum{ A[i]*S[i] for i = 0..N−1 }|
//(Assume that the sum of zero elements equals zero.)
//For a given array A, we are looking for such a sequence S that minimizes val(A,S).
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given an array A of N integers, computes the minimum value of val(A,S) from all possible values of val(A,S) for all possible sequences S of N integers from the set {−1, 1}.
//For example, given array:
//  A[0] =  1  
//  A[1] =  5  
//  A[2] =  2  
//  A[3] = -2  
//your function should return 0, since for S = [−1, 1, −1, 1], val(A, S) = 0, which is the minimum possible value.
//Assume that:
//N is an integer within the range [0..20,000];
//each element of array A is an integer within the range [−100..100].
//Complexity:
//expected worst-case time complexity is O(N*max(abs(A))2);
//expected worst-case space complexity is O(N+sum(abs(A))), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package dynamic_programming

import scala.math._

object MinAbsSum {
  def solution(A: Array[Int]): Int = {
    val N = A.length
    var M = 0
    for (i <- 0 until N) {
      A(i) = abs(A(i))
      M = max(A(i), M)
    }
    var S = A.sum
    var count = new Array[Int](M + 1)
    for (i <- 0 until N)
      count(A(i)) += 1
    var dp = new Array[Int](S + 1)
    //dp(0)的值为0
    //dp[j]表示只使用前i种数，达到总和j的时候，最多还能剩余多少个第i种数
    for (i <- 1 to S)
      dp(i) = -1
    for (i <- 1 to M) {
      if (count(i) > 0) {
        for (j <- 0 until S) {
          //Can be reached with previous numbers
          if (dp(j) >= 0)
            dp(j) = count(i)
          // Cannot be reached with only previous numbers.
          // But could be achieved with previous numbers AND current one.
          else if (j >= i && dp(j - i) > 0)
            dp(j) = dp(j - i) - 1
        }
      }
    }
    var re = S
    for (i <- 0 to S / 2)
      if (dp(i) >= 0)
        re = min(re, S - 2 * i)
    re
  }
}