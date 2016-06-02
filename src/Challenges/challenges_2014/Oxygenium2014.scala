//An integer K and a non-empty zero-indexed array A consisting of N integers are given.
//A pair of integers (P, Q), such that 0 ≤ P ≤ Q < N, is called a slice of array A.
//A bounded slice is a slice in which the difference between the maximum and minimum values in the slice is less than or equal to K. More precisely it is a slice, such that max(A[P], A[P + 1], ..., A[Q]) − min(A[P], A[P + 1], ..., A[Q]) ≤ K.
//The goal is to calculate the number of bounded slices.
//For example, consider K = 2 and array A such that:
//    A[0] = 3
//    A[1] = 5
//    A[2] = 7
//    A[3] = 6
//    A[4] = 3
//There are exactly nine bounded slices: (0, 0), (0, 1), (1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3), (4, 4).
//Write a function:
//object Solution { def solution(K: Int, A: Array[Int]): Int }
//that, given an integer K and a non-empty zero-indexed array A of N integers, returns the number of bounded slices of array A.
//If the number of bounded slices is greater than 1,000,000,000, the function should return 1,000,000,000.
//For example, given:
//    A[0] = 3
//    A[1] = 5
//    A[2] = 7
//    A[3] = 6
//    A[4] = 3
//the function should return 9, as explained above.
//Assume that:
//N is an integer within the range [1..100,000];
//K is an integer within the range [0..1,000,000,000];
//each element of array A is an integer within the range [−1,000,000,000..1,000,000,000].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Challenges.challenges_2014

//此题需用数组，使用List超时
object Oxygenium2014 {
  def main(args: Array[String]) {
    val K = 2
    val A = Array(3, 5, 7, 6, 3)
    println(solution(K, A))
  }

  def solution(K: Int, A: Array[Int]): Int = {
    val N = A.length
    var qmin = new Array[Int](N)
    var qmax = new Array[Int](N)
    var firstmin = 0
    var lastmin = -1
    var firstmax = 0
    var lastmax = -1
    var ans = 0
    var j = 0
    for (i <- 0 until N) {
      var flag = true
      while (j < A.length && flag) {
        while (lastmin >= firstmin && A(qmin(lastmin)) >= A(j))
          lastmin -= 1
        lastmin += 1
        qmin(lastmin) = j
        while (lastmax >= firstmax && A(qmax(lastmax)) <= A(j))
          lastmax -= 1
        lastmax += 1
        qmax(lastmax) = j
        if (A(qmax(firstmax)) - A(qmin(firstmin)) <= K)
          j += 1
        else
          flag = false
      }
      if (qmin(firstmin) == i)
        firstmin += 1
      if (qmax(firstmax) == i)
        firstmax += 1
      ans += j - i
      if (ans >= 1000000000)
        return 1000000000
    }
    ans
  }
}