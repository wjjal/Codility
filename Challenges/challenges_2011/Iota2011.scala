//A non-empty zero-indexed array A consisting of N integers is given. Two integers P and Q are called adjacent in array A if there exists an index 0 ≤ K < N−1 such that:
//P = A[K] and Q = A[K+1], or
//Q = A[K] and P = A[K+1].
//A non-empty zero-indexed sequence B consisting of M integers is called adjacent in array A if the following conditions hold:
//B[0] = A[0];
//B[M−1] = A[N−1];
//B[K] and B[K+1] are adjacent in A for 0 ≤ K < M−1.
//For example, consider array A consisting of eight elements such that:
//A[0] = 1    A[1] = 10    A[2] = 6
//A[3] = 5    A[4] = 10    A[5] = 7
//A[6] = 5    A[7] = 2
//The following sequences are adjacent in array A:
//[1, 10, 6, 5, 10, 7, 5, 2];
//[1, 10, 7, 5, 2];
//[1, 10, 6, 5, 10, 6, 5, 10, 7, 5, 2];
//[1, 10, 5, 2].
//The last sequence is the shortest possible sequence adjacent in array A.
//Write a function
//object Solution { def solution(A: Array[Int]): Int }
//that, given a non-empty zero-indexed array A consisting of N integers, returns the length of the shortest sequence adjacent in array A.
//Assume that:
//N is an integer within the range [1..100,000];
//each element of array A is an integer within the range [−2,147,483,648..2,147,483,647].
//For example, given array A such that:
//A[0] = 1    A[1] = 10    A[2] = 6
//A[3] = 5    A[4] = 10    A[5] = 7
//A[6] = 5    A[7] = 2
//the function should return 4, as explained above.
//Complexity:
//expected worst-case time complexity is O(N*log(N));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges_2011

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

object iota2011 {
  def main(args: Array[String]) {
    val A = Array(1, 10, 6, 5, 10, 7, 5, 2)
    println(solution(A))
  }

  def solution(A: Array[Int]): Int = {
    var N = A.length
    if (A(0) == A(N - 1))
      return 1
    var edge = new HashMap[Int, HashSet[Int]]()
    for (i <- 1 until N) {
      if (edge.contains(A(i)))
        edge(A(i)).+=(A(i - 1))
      else
        edge.+=((A(i), HashSet(A(i - 1))))
      if (edge.contains(A(i - 1)))
        edge(A(i - 1)).+=(A(i))
      else
        edge.+=((A(i - 1), HashSet(A(i))))
    }
    var queue = new Queue[Int]()
    var len = 1
    var mark = new HashSet[Int]()
    queue.+=(A(0))
    while (!queue.isEmpty) { {
        len += 1
        for (t <- 0 until queue.length) {
          var x = queue.dequeue
          mark.+=(x)
          var set = edge(x)
          for (p <- set) {
            if (p == A(N - 1))
              return len
            if (!mark.contains(p)) {
              queue.+=(p)
              mark.+=(p)
            }
          }
        }
      }
    }
    len
  }
}