//You are given N round clocks.
//Every clock has M hands, and these hands can point to positions 1, 2, 3, ..., P (yes, these represent numbers around each face). The clocks are represented by the matrix A consisting of N rows and M columns of integers. The first row represents the hands of the first clock, and so on.
//For example, you are given matrix A consisting of five rows and two columns, and P = 4:
//  A[0][0] = 1    A[0][1] = 2
//  A[1][0] = 2    A[1][1] = 4
//  A[2][0] = 4    A[2][1] = 3
//  A[3][0] = 2    A[3][1] = 3
//  A[4][0] = 1    A[4][1] = 3
//
//You can rotate the clocks to obtain several clocks that look identical. For example, if you rotate the third, fourth and fifth clocks you can obtain the following clocks:
//
//After rotation, you have four pairs of clocks that look the same: (1, 3), (1, 4), (2, 5) and (3, 4).
//Write a function:
//object Solution { def solution(A: Array[Array[Int]], P: Int): Int }
//that, given a zero-indexed matrix A consisting of N rows and M columns of integers and integer P, returns the maximal number of pairs of clocks that can look the same after rotation.
//For example, given the following array A and P = 4:
//    A[0][0] = 1     A[0][1] = 2
//    A[1][0] = 2     A[1][1] = 4
//    A[2][0] = 4     A[2][1] = 3
//    A[3][0] = 2     A[3][1] = 3
//    A[4][0] = 1     A[4][1] = 3
//the function should return 4, as explained above.
//Assume that:
//N and M are integers within the range [1..500];
//P is an integer within the range [1..1,000,000,000];
//each element of matrix A is an integer within the range [1..P];
//the elements of each row of matrix A are all distinct.
//Complexity:
//expected worst-case time complexity is O(N*M*log(N+M));
//expected worst-case space complexity is O(N*M).

package challenges

import scala.math._
import scala.collection.mutable.ArrayBuffer

object Lithium2013 {
  def main(args: Array[String]) {
    val A = new Array[Array[Int]](10)
    A(0) = Array(22, 8 ,60, 35, 54, 32, 63, 43, 44, 33)
    A(1) = Array(6 ,45 ,59 ,1 ,52 ,41, 49, 58 ,53 ,2)
     A(2) = Array(45 ,52 ,62, 5, 44, 63, 9, 43, 18, 50)
     A(3) = Array(30 ,1 ,5, 25, 26, 12, 19, 18, 13, 9)
    A(4) = Array(33, 61, 38, 32, 50, 57, 31, 51, 40, 6 )
    A(5) = Array(39 ,57 ,40 ,45 ,46 ,52 ,53 ,36 ,28, 32 )
    A(6) = Array(36 ,26, 48, 3, 39, 64, 12, 58, 47, 37)
    A(7) = Array(32 ,56 ,4 ,3 ,23 ,20 ,14 ,46 ,57 ,59 )
    A(8) = Array(17 ,6 ,59, 36, 16, 27, 45, 33, 5, 8)
    A(9) = Array(8 ,51 ,60 ,43 ,54 ,47 ,55 ,3 ,61, 4)
  
    val P = 64
    println(solution(A, P))
  }

  def solution(A: Array[Array[Int]], P: Int): Int = {
    val N = A.length
    val M = A(0).length
    if (M == 1)
      return N * (N - 1) / 2
    var same_after_rotation = new ArrayBuffer[String]()
    var ans = 0
    for (i <- 0 until N) {
      A(i) = A(i).sorted
      var distance = new Array[Int](M)
      distance(0) = (A(i)(0)+P - A(i)(M - 1)) % P
      for (j <- 1 until M)
        distance(j) = (A(i)(j) - A(i)(j - 1)) % P
      same_after_rotation.+=(lexicograhically_min(distance))
    }
    same_after_rotation = same_after_rotation.sorted
    var begin = 0
    var end = 0
    for (i <- 1 until N) {
      if (same_after_rotation(i) == (same_after_rotation(i - 1))) {
        end += 1
      } else {
        ans += (end - begin + 1) * (end - begin + 1 - 1) / 2
        begin = i
        end = i
      }
    }
    if (N > 2 && same_after_rotation(N - 1) == same_after_rotation(N - 2))
      ans += (end - begin + 1) * (end - begin + 1 - 1) / 2
    ans
  }

  def lexicograhically_min(A: Array[Int]): String = {
    var double_array = A ++ A
    var array_len = A.length
    var start = 0
    var testing = 1
    var offset = 0
    while (testing < array_len && offset != array_len) {
      if (double_array(start + offset) == double_array(testing + offset))
        offset += 1
      else if (double_array(start + offset) < double_array(testing + offset)) {
        testing += offset + 1
        offset = 0
      } else {
        start = max(start + offset + 1, testing)
        testing = start + 1
        offset = 0
      }
    }
    return (A.drop(start) ++ A.dropRight(array_len - start)).mkString
  }
}