//An integer M and a non-empty zero-indexed array A consisting of N non-negative integers are given. All integers in array A are less than or equal to M.
//A pair of integers (P, Q), such that 0 ≤ P ≤ Q < N, is called a slice of array A. The slice consists of the elements A[P], A[P + 1], ..., A[Q]. A distinct slice is a slice consisting of only unique numbers. That is, no individual number occurs more than once in the slice.
//For example, consider integer M = 6 and array A such that:
//    A[0] = 3
//    A[1] = 4
//    A[2] = 5
//    A[3] = 5
//    A[4] = 2
//There are exactly nine distinct slices: (0, 0), (0, 1), (0, 2), (1, 1), (1, 2), (2, 2), (3, 3), (3, 4) and (4, 4).
//The goal is to calculate the number of distinct slices.
//Write a function:
//object Solution { def solution(M: Int, A: Array[Int]): Int }
//that, given an integer M and a non-empty zero-indexed array A consisting of N integers, returns the number of distinct slices.
//If the number of distinct slices is greater than 1,000,000,000, the function should return 1,000,000,000.
//For example, given integer M = 6 and array A such that:
//    A[0] = 3
//    A[1] = 4
//    A[2] = 5
//    A[3] = 5
//    A[4] = 2
//the function should return 9, as explained above.
//Assume that:
//N is an integer within the range [1..100,000];
//M is an integer within the range [0..100,000];
//each element of array A is an integer within the range [0..M].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(M), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Lessons.Lesson15_Caterpillar_method

import scala.math._

object CountDistinctSlices {
  def solution(M: Int, A: Array[Int]): Int = {
    val N = A.length
    var result = 0
    var begin = 0
    var end = 0
    var count = new Array[Int](M+1)
    count = count.map(_ - 1)
    for (end <- 0 until N) {
      if (count(A(end)) == -1) {
        count(A(end)) = end
      } else {
        var newBegin = count(A(end)) + 1
        result += (newBegin - begin) * (end - begin + end - newBegin + 1) / 2
        if (result >= 1000000000)
          return 1000000000
        for (k <- begin until newBegin)
          count(A(k)) = -1
        count(A(end)) = end
        begin = newBegin
      }
    }
    end = N - 1
    //若无此判断，codility上会有溢出
    if(1000000000*2/(end - begin + 1)<(end - begin + 1 + 1))
      return 1000000000
    result += (end - begin + 1) * (end - begin + 1 + 1) / 2
    min(result, 1000000000)
  }
}