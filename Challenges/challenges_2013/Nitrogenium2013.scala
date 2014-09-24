//The city of Codicity is located at the seaside. The city area comprises N plots located along a boulevard on one side of the city. Each plot is flat, but different plots have different heights above the sea level. The relative heights of the consecutive plots are given in the form of a non-empty zero-indexed array A of N integers.
//The sea level changes constantly and many plots are sometimes under water. Water levels on consecutive days are given in the form of a non-empty zero-indexed array B of M integers.
//A slice of array A is any pair of integers (P, Q) such that 0 ≤ P ≤ Q < N. An island is a slice of consecutive plots that rise above the water’s surface. The plots on either side of each island are under water. More precisely, if the level of the water is K, then an island is a slice (P, Q) in which the level of each plot A[P], A[P + 1], ..., A[Q] is greater than K. Both of the adjacent plots should also be under water; that is:
//P = 0 or A[P − 1] ≤ K
//Q = N − 1 or A[Q + 1] ≤ K
//The goal is to calculate the number of islands on consecutive days.
//For example, given the following arrays A and B:
//    A[0] = 2    B[0] = 0
//    A[1] = 1    B[1] = 1
//    A[2] = 3    B[2] = 2
//    A[3] = 2    B[3] = 3
//    A[4] = 3    B[4] = 1
//We have the following number of islands on consecutive days:
//on the first day there is only 1 island: (0, 4),
//on the second day there are 2 islands: (0, 0) and (2, 4),
//on the third day there are 2 islands: (2, 2) and (4, 4),
//on the fourth day there aren't any islands,
//on the fifth day there are 2 islands: (0, 0) and (2, 4).
//Write a function:
//object Solution { def solution(A: Array[Int], B: Array[Int]): Array[Int] }
//that, given a non-empty zero-indexed array A of N integers and a non-empty zero-indexed array B of M integers, returns a sequence consisting of M integers representing the number of islands on consecutive days.
//The sequence should be returned as:
//a structure Results (in C), or
//a vector of integers (in C++), or
//a record Results (in Pascal), or
//an array of integers (in any other programming language).
//For example, given:
//    A[0] = 2    B[0] = 0
//    A[1] = 1    B[1] = 1
//    A[2] = 3    B[2] = 2
//    A[3] = 2    B[3] = 3
//    A[4] = 3    B[4] = 1
//the function should return the array [1, 2, 2, 0, 2], as explained above.
//Assume that:
//N and M are integers within the range [1..30,000];
//each element of arrays A, B is an integer within the range [0..100,000].
//Complexity:
//expected worst-case time complexity is O(N+M+max(A)+max(B));
//expected worst-case space complexity is O(N+M+max(A)+max(B)), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges_2013

import scala.math._

//Imagine that the water level only decreases on consecutive days, and that on the first day
//all the plots are under water, so there aren’t any islands. Next, we will investigate how the
//number of islands changes depending on the water level. More precisely, we want to know
//the value by which the number of islands increases or decreases if the water level decreases
//to some level.
//Each island has two sides (shores): left and right. Instead of counting the islands, we can
//count their right sides. A position j is the right side of an island only if:
//• Aj > Aj+1 (or j = N − 1),
//• the water level is below Aj , but not below Aj+1.
//So for every position j, such that Aj > Aj+1 (or j = N − 1), when the water level falls below
//Aj the number of islands increases by one, but when it falls below Aj+1 it decreases by one.
//For any water level, the number of islands is just the sum of changes in the number of
//islands as the water level was decreasing.

object Nitrogenium2013 {
  def main(args: Array[String]) {
    val A = Array(2, 1, 3, 2, 3)
    val B = Array(0, 1, 2, 3, 1)
    val re = solution(A, B)
    for (elem <- re)
      println(elem)
  }

  def solution(A: Array[Int], B: Array[Int]): Array[Int] = {
    val N = A.length
    val M = B.length
    var size = max(A.max, B.max)
    var island = new Array[Int](size + 2)
    for (i <- 1 until N) {
      if (A(i - 1) > A(i)) {
        island(A(i - 1)) += 1
        island(A(i)) -= 1
      }
    }
    island(A(N - 1)) += 1
    for (i <- size to (0, -1)) {
      island(i) += island(i + 1)
    }
    var re = new Array[Int](M)
    for (i <- 0 until M)
      re(i) = island(B(i) + 1)
    re
  }
}