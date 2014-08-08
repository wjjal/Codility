//A non-empty zero-indexed array A consisting of N integers is given. A pair of integers (P, Q), such that 0 ≤ P ≤ Q < N, is called a slice of array A. The sum of a slice (P, Q) is the total of A[P] + A[P+1] + ... + A[Q]. The maximum sum is the maximum sum of any slice of A.
//For example, consider array A such that:
//    A[0] = 3
//    A[1] = 2
//    A[2] = -6
//    A[3] = 3
//    A[4] = 1
//For example (0, 1) is a slice of A that has sum A[0] + A[1] = 5. This is the maximum sum of A.
//You can perform a single swap operation in array A. This operation takes two indices I and J, such that 0 ≤ I ≤ J < N, and exchanges the values of A[I] and A[J]. The goal is to find the maximum sum you can achieve after performing a single swap.
//For example, after swapping elements 2 and 4, you will get the following array A:
//    A[0] = 3
//    A[1] = 2
//    A[2] = 1
//    A[3] = 3
//    A[4] = -6
//After that, (0, 3) is a slice of A that has the sum A[0] + A[1] + A[2] + A[3] = 9. This is the maximum possible sum of A after a single swap.
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given a non-empty zero-indexed array A of N integers, returns the maximum possible sum of any slice of A after a single swap operation.
//For example, given:
//    A[0] = 3
//    A[1] = 2
//    A[2] = -6
//    A[3] = 3
//    A[4] = 1
//the function should return 9, as explained above.
//Assume that:
//N is an integer within the range [1..100,000];
//each element of array A is an integer within the range [−10,000..10,000].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges

import scala.math._

object Aluminium2014 {
  def solution(A: Array[Int]): Int = {
    max(findMaxWithASwap(A), findMaxWithASwap(A.reverse))
  }

  def findMaxWithASwap(a: Array[Int]): Int = {
    val N = a.length
    var left = new Array[Int](N)
    left(0) = a(0)
    var temp = a(0)
    for (i <- 1 until N) {
      //temp为a(0)到a(i)最大值
      temp = max(temp, a(i))
      //left(i)为到a(i)为止，允许一次交换的最大和，或者交换发生在i处(把a(i)与之前的最大值temp交换，此时left(i)=temp
      //或者交换发生在i之前，即i处不发生交换，只需把i-1时计算的最大和加上a(i)即可，即left(i)=left(i-1)+a(i))
      left(i) = max(temp, left(i - 1) + a(i))
    }

    var right = new Array[Int](N)
    right(N - 1) = a(N - 1)
    var re = a(N - 1)
    //计算传统连续子段最大和，倒序计算
    for (i <- (0 until N-1).reverse) {
      right(i) = max(right(i + 1), 0) + a(i)
      re = max(right(i), re)
    }
    for (i <- 1 until N) {
      re = max(re, right(i) + left(i - 1) - a(i))
    }
    re
  }
}