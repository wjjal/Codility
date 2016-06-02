//We say that the set of segments is non-overlapping if it contains no two overlapping segments. The goal is to find the size of a non-overlapping set containing the maximal number of segments.
//For example, consider arrays A, B such that:
//    A[0] = 1    B[0] = 5
//    A[1] = 3    B[1] = 6
//    A[2] = 7    B[2] = 8
//    A[3] = 9    B[3] = 9
//    A[4] = 9    B[4] = 10
//The segments are shown in the figure below.
//
//The size of a non-overlapping set containing a maximal number of segments is 3. For example, possible sets are {0, 2, 3}, {0, 2, 4}, {1, 2, 3} or {1, 2, 4}. There is no non-overlapping set with four segments.
//Write a function:
//object Solution { def solution(A: Array[Int], B: Array[Int]): Int }
//that, given two zero-indexed arrays A and B consisting of N integers, returns the size of a non-overlapping set containing a maximal number of segments.
//For example, given arrays A, B shown above, the function should return 3, as explained above.
//Assume that:
//N is an integer within the range [0..30,000];
//each element of arrays A, B is an integer within the range [0..1,000,000,000];
//A[I] ≤ B[I], for each I (0 ≤ I < N);
//B[K] ≤ B[K + 1], for each K (0 ≤ K < N − 1).
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.
package Lessons.Lesson16_Greedy_algorithms

object MaxNonoverlappingSegments {
  def solution(A: Array[Int], B: Array[Int]): Int = {
    val N = A.length
    if (N == 0)
      return 0
    var pair = new Array[(Int, Int)](N)
    for (i <- 0 until N)
      pair(i) = (A(i), B(i))
    pair = pair.sortBy(_._2)
    var end = pair(0)._2
    var re = 1
    for (i <- 1 until N) {
      if (pair(i)._1 > end) {
        re = re + 1
        end = pair(i)._2
      }
    }
    return re
  }
}