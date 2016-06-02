//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given a non-empty zero-indexed array A of N integers, returns the minimal positive integer that does not occur in A.
//For example, given:
//  A[0] = 1
//  A[1] = 3
//  A[2] = 6
//  A[3] = 4
//  A[4] = 1
//  A[5] = 2
//the function should return 5.
//Assume that:
//N is an integer within the range [1..100,000];
//each element of array A is an integer within the range [−2,147,483,648..2,147,483,647].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Lessons.Lesson4_Counting_Elements

object MissingInteger {
  def solution(A: Array[Int]): Int = {
    val N = A.length
    var flags = new Array[Boolean](N)
    for (elem <- A) {
      if (elem <= N && elem > 0)
        flags(elem - 1) = true
    }
    for (i <- 0 until N) {
      if (!flags(i))
        return i + 1
    }
    N + 1
  }
}