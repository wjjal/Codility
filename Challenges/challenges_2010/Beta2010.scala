//Given an array A of N integers, we draw N discs in a 2D plane such that the I-th disc is centered on (0,I) and has a radius of A[I]. We say that the J-th disc and K-th disc intersect if J ≠ K and J-th and K-th discs have at least one common point.
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given an array A describing N discs as explained above, returns the number of pairs of intersecting discs. For example, given N=6 and:
//A[0] = 1  A[1] = 5  A[2] = 2 
//A[3] = 1  A[4] = 4  A[5] = 0  
//intersecting discs appear in eleven pairs of elements:
//0 and 1,
//0 and 2,
//0 and 4,
//1 and 2,
//1 and 3,
//1 and 4,
//1 and 5,
//2 and 3,
//2 and 4,
//3 and 4,
//4 and 5.
//so the function should return 11.
//The function should return −1 if the number of intersecting pairs exceeds 10,000,000.
//Assume that:
//N is an integer within the range [0..100,000];
//each element of array A is an integer within the range [0..2147483647].
//Complexity:
//expected worst-case time complexity is O(N*log(N));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges_2010

object Beta2010 {
  def main(args: Array[String]) {
    val A = Array(1, 1, 1)
    println(solution(A))
  }

  def solution(A: Array[Int]): Int = {
    val N = A.length
    var mark = new Array[(Long, Int)](2 * N)
    for (i <- 0 until N) {
      //进入和出去同时的话，先考虑进再考虑出，为了排序方便，让进为-1,出为1
      mark(2 * i) = (i.toLong - A(i), -1)
      mark(2 * i + 1) = (i.toLong + A(i), 1)
    }
    mark = mark.sortBy(x => (x._1, x._2))
    var re = 0
    var cur = 0
    for (i <- 0 until 2 * N) {
      if (mark(i)._2 < 0) {
        re += cur
        cur += 1
        if (re > 10000000)
          return -1
      } else
        cur -= 1
    }
    re
  }
}