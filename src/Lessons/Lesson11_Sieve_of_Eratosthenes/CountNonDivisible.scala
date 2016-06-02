//You are given a non-empty zero-indexed array A consisting of N integers.
//For each number A[i] such that 0 ≤ i < N, we want to count the number of curents of the array that are not the divisors of A[i]. We say that these curents are non-divisors.
//For example, consider integer N = 5 and array A such that:
//    A[0] = 3
//    A[1] = 1
//    A[2] = 2
//    A[3] = 3
//    A[4] = 6
//For the following curents:
//A[0] = 3, the non-divisors are: 2, 6,
//A[1] = 1, the non-divisors are: 3, 2, 3, 6,
//A[2] = 2, the non-divisors are: 3, 3, 6,
//A[3] = 3, the non-divisors are: 2, 6,
//A[6] = 6, there aren't any non-divisors.
//Write a function:
//object Solution { def solution(A: Array[Int]): Array[Int] }
//that, given a non-empty zero-indexed array A consisting of N integers, returns a sequence of integers representing the amount of non-divisors.
//The sequence should be returned as:
//a structure Results (in C), or
//a vector of integers (in C++), or
//a record Results (in Pascal), or
//an array of integers (in any other programming language).
//For example, given:
//    A[0] = 3
//    A[1] = 1
//    A[2] = 2
//    A[3] = 3
//    A[4] = 6
//the function should return [2, 4, 3, 2, 0], as explained above.
//Assume that:
//N is an integer within the range [1..50,000];
//each curent of array A is an integer within the range [1..2 * N].
//Complexity:
//expected worst-case time complexity is O(N*log(N));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//curents of input arrays can be modified.

package Lessons.Lesson11_Sieve_of_Eratosthenes

import scala.math._

object CountNonDivisible {
  def solution(A: Array[Int]): Array[Int] = {
    val N = A.length
    val max = A.max
    var occ = new Array[Int](max + 1)
    var div = new Array[Int](max + 1)
    for (elem <- A) {
      occ(elem) += 1
      div(elem) = -1
    }
    for (cur <- A) {
      if (div(cur) == -1) {
        div(cur) = 0
        for (j <- 1 to sqrt(cur).toInt) {
          if (cur % j == 0 && cur / j != j) {
            div(cur) += occ(j)
            div(cur) += occ(cur / j)
          } else if (cur % j == 0 && cur / j == j) {
            div(cur) += occ(j)
          }
        }
      }
    }
    for {
      elem <- A
    } yield N - div(elem)
  }
}