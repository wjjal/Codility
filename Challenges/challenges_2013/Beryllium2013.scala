//A non-empty zero-indexed array A consisting of N integers is given. A prefix suffix set is a pair of indices (P, S) such that 0 ≤ P, S < N and such that:
//every value that occurs in the sequence A[0], A[1], ..., A[P] also occurs in the sequence A[S], A[S + 1], ..., A[N − 1],
//every value that occurs in the sequence A[S], A[S + 1], ..., A[N − 1] also occurs in the sequence A[0], A[1], ..., A[P].
//The goal is to calculate the number of prefix suffix sets in the array.
//For example, consider array A such that:
//    A[0] = 3
//    A[1] = 5
//    A[2] = 7
//    A[3] = 3
//    A[4] = 3
//    A[5] = 5
//There are exactly fourteen prefix suffix sets: (1, 4), (1, 3), (2, 2), (2, 1), (2, 0), (3, 2), (3, 1), (3, 0), (4, 2), (4, 1), (4, 0), (5, 2), (5, 1), (5, 0).
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given a non-empty zero-indexed array A of N integers, returns the number of prefix suffix sets.
//If the number of prefix suffix sets is greater than 1,000,000,000, the function should return 1,000,000,000.
//For example, given:
//    A[0] = 3
//    A[1] = 5
//    A[2] = 7
//    A[3] = 3
//    A[4] = 3
//    A[5] = 5
//the function should return 14, as explained above.
//Assume that:
//N is an integer within the range [1..40,000];
//each element of array A is an integer within the range [−1,000,000,000..1,000,000,000].
//Complexity:
//expected worst-case time complexity is O(N*log(N));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Challenges.challenges_2013

import scala.collection.mutable.HashMap
import scala.math._

object Beryllium2013 {
  def main(args: Array[String]) {
    val A = Array(3, 5, 7, 3, 3, 5)
    println(solution(A))
  }

  def solution(A: Array[Int]): Int = {
    val N = A.length
    var first = new HashMap[Int, Int]()
    var last = new HashMap[Int, Int]()
    for (i <- 0 until N)
      last(A(i)) = i
    for (i <- N - 1 to (0, -1))
      first(A(i)) = i
    var i = 0
    var j = N - 1
    var re = 0
    while (i < N || j >= 0) {
      var p = last(A(i))
      while (j > p) {
        var q = first(A(j))
        while (i < q) {
          i += 1
          p = min(p, last(A(i)))
        }
        j-=1
      }
      var x = i + 1
      while (x < N && last(A(x)) >= j)
        x += 1
      var y = j - 1
      while (y >= 0 && first(A(y)) <= i)
        y -= 1
      re += (x - i) * (j - y)
      if (re >= 1000000000)
        return 1000000000
      i = x
      j = y
    }
    re
  }
}