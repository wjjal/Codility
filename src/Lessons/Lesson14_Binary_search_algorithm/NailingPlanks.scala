//You are given two non-empty zero-indexed arrays A and B consisting of N integers. These arrays represent N planks. More precisely, A[K] is the start and B[K] the end of the K−th plank.
//Next, you are given a non-empty zero-indexed array C consisting of M integers. This array represents M nails. More precisely, C[I] is the position where you can hammer in the I−th nail.
//We say that a plank (A[K], B[K]) is nailed if there exists a nail C[I] such that A[K] ≤ C[I] ≤ B[K].
//The goal is to find the minimum number of nails that must be used until all the planks are nailed. In other words, you should find a value J such that all planks will be nailed after using only the first J nails. More precisely, for every plank (A[K], B[K]) such that 0 ≤ K < N, there should exist a nail C[I] such that I < J and A[K] ≤ C[I] ≤ B[K].
//For example, given arrays A, B such that:
//    A[0] = 1    B[0] = 4
//    A[1] = 4    B[1] = 5
//    A[2] = 5    B[2] = 9
//    A[3] = 8    B[3] = 10
//four planks are represented: [1, 4], [4, 5], [5, 9] and [8, 10].
//Given array C such that:
//    C[0] = 4
//    C[1] = 6
//    C[2] = 7
//    C[3] = 10
//    C[4] = 2
//if we use the following nails:
//0, then planks [1, 4] and [4, 5] will both be nailed.
//0, 1, then planks [1, 4], [4, 5] and [5, 9] will be nailed.
//0, 1, 2, then planks [1, 4], [4, 5] and [5, 9] will be nailed.
//0, 1, 2, 3, then all the planks will be nailed.
//Thus, four is the minimum number of nails that, used sequentially, allow all the planks to be nailed.
//Write a function:
//object Solution { def solution(A: Array[Int], B: Array[Int], C: Array[Int]): Int }
//that, given two non-empty zero-indexed arrays A and B consisting of N integers and a non-empty zero-indexed array C consisting of M integers, returns the minimum number of nails that, used sequentially, allow all the planks to be nailed.
//If it is not possible to nail all the planks, the function should return −1.
//For example, given arrays A, B, C such that:
//    A[0] = 1    B[0] = 4
//    A[1] = 4    B[1] = 5
//    A[2] = 5    B[2] = 9
//    A[3] = 8    B[3] = 10
//
//    C[0] = 4
//    C[1] = 6
//    C[2] = 7
//    C[3] = 10
//    C[4] = 2
//the function should return 4, as explained above.
//Assume that:
//N and M are integers within the range [1..30,000];
//each element of arrays A, B, C is an integer within the range [1..2*M];
//A[K] ≤ B[K].
//Complexity:
//expected worst-case time complexity is O((N+M)*log(M));
//expected worst-case space complexity is O(M), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

//参考http://codesays.com/2014/solution-to-nailing-planks-by-codility/的解答

package Lessons.Lesson14_Binary_search_algorithm

import scala.collection.immutable.IndexedSeq
import scala.math._

object NailingPlanks {
  def solution(A: Array[Int], B: Array[Int], C: Array[Int]): Int = {
    val N = A.length
    val M = C.length
    var i = -1
    var nails = for { i <- 0 until M } yield (i, C(i))
    nails = nails.sortBy(_._2)
    var result = -1
    for (i <- 0 until N) {
      result = findFirstNail(A(i), B(i), nails, result)
      if (result == -1)
        return -1
    }
    result + 1
  }
  def findFirstNail(begin: Int, end: Int, nails: IndexedSeq[(Int, Int)], preresult: Int): Int = {
    val M = nails.length
    var result = -1
    var resultpos = -1
    var left = 0
    var right = M - 1
    while (left <= right) {
      var mid = (left + right) >> 1
      var nailmidpos = nails(mid)._2
      if (nailmidpos < begin)
        left = mid + 1
      else if (nailmidpos > end)
        right = mid - 1
      else {
        right = mid - 1
        result = nails(mid)._1
        resultpos = mid
      }
    }
    if (result == -1)
      return -1
    resultpos += 1
    var flag = true
    //Linear search all the quanlified nails, and find
    // out the one with the earliest position.
    while (resultpos < M && flag) {
      if (nails(resultpos)._2 > end)
        flag = false
      if (flag) {
        result = min(result, nails(resultpos)._1)
        resultpos += 1
      }
      if (preresult >= result)
        return preresult
    }
    max(result, preresult)
  }
}