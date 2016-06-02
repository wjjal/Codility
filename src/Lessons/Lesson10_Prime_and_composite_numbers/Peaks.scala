//A non-empty zero-indexed array A consisting of N integers is given.
//A peak is an array element which is larger than its neighbors. More precisely, it is an index P such that 0 < P < N − 1,  A[P − 1] < A[P] and A[P] > A[P + 1].
//For example, the following array A:
//    A[0] = 1
//    A[1] = 2
//    A[2] = 3
//    A[3] = 4
//    A[4] = 3
//    A[5] = 4
//    A[6] = 1
//    A[7] = 2
//    A[8] = 3
//    A[9] = 4
//    A[10] = 6
//    A[11] = 2
//has exactly three peaks: 3, 5, 10.
//We want to divide this array into blocks containing the same number of elements. More precisely, we want to choose a number K that will yield the following blocks:
//A[0], A[1], ..., A[K − 1],
//A[K], A[K + 1], ..., A[2K − 1],
//...
//A[N − K], A[N − K + 1], ..., A[N − 1].
//What's more, every block should contain at least one peak. Notice that extreme elements of the blocks (for example A[K − 1] or A[K]) can also be peaks, but only if they have both neighbors (including one in an adjacent blocks).
//The goal is to find the maximum number of blocks into which the array A can be divided.
//Array A can be divided into blocks as follows:
//one block (1, 2, 3, 4, 3, 4, 1, 2, 3, 4, 6, 2). This block contains three peaks.
//two blocks (1, 2, 3, 4, 3, 4) and (1, 2, 3, 4, 6, 2). Every block has a peak.
//three blocks (1, 2, 3, 4), (3, 4, 1, 2), (3, 4, 6, 2). Every block has a peak. Notice in particular that the first block (1, 2, 3, 4) has a peak at A[3], because A[2] < A[3] > A[4], even though A[4] is in the adjacent block.
//However, array A cannot be divided into four blocks, (1, 2, 3), (4, 3, 4), (1, 2, 3) and (4, 6, 2), because the (1, 2, 3) blocks do not contain a peak. Notice in particular that the (4, 3, 4) block contains two peaks: A[3] and A[5].
//The maximum number of blocks that array A can be divided into is three.
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given a non-empty zero-indexed array A consisting of N integers, returns the maximum number of blocks into which A can be divided.
//If A cannot be divided into some number of blocks, the function should return 0.
//For example, given:
//    A[0] = 1
//    A[1] = 2
//    A[2] = 3
//    A[3] = 4
//    A[4] = 3
//    A[5] = 4
//    A[6] = 1
//    A[7] = 2
//    A[8] = 3
//    A[9] = 4
//    A[10] = 6
//    A[11] = 2
//the function should return 3, as explained above.
//Assume that:
//N is an integer within the range [1..100,000];
//each element of array A is an integer within the range [0..1,000,000,000].
//Complexity:
//expected worst-case time complexity is O(N*log(log(N)));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

//考虑距离最远的两个山峰，设最大距离为D，（第一个山峰到开头的距离，以及最后一个山峰到末尾的距离都算进去），
//则如果桶的size >= D，则一定可以。size < D / 2一定不可以。
//所以我们只要看D/2..D之间的n的约数即可。如果这里没有解，再沿着D向上找一个约数（但是不用再验证）。
//之所以把第一个山峰和最后一个山峰那块都算进去是因为可以保证第一个桶和最后一个桶不空。然后可以轻松得出上述结论。
//复杂度分析，令m = D / 2。
//则 复杂度只有n / m + n / (m + 1) + ... + n / D，每一项不大于n / m，一共m项，所以时间复杂度居然是O(n)。

package Lessons.Lesson10_Prime_and_composite_numbers

import scala.math._

object Peaks {
  def solution(A: Array[Int]): Int = {
    val N = A.length
    if (N <= 2)
      return 0
    var sum = new Array[Int](N)
    var last = -1
    var D = 0
    for (i <- 1 to N - 2) {
      sum(i) = sum(i - 1)
      if (A(i) > A(i - 1) && A(i) > A(i + 1)) {
        D = max(D, i - last)
        last = i
        sum(i) += 1
      }
    }
    if (sum(N - 2) == 0) {
      return 0;
    }
    sum(N - 1) = sum(N - 2)
    D = max(D, N - last);
    for (i <- (D >> 1) + 1 until D) {
      if (N % i == 0) {
        last = 0;
        var j = i
        var flag = true
        while (j <= N && flag) {
          if (sum(j - 1) <= last)
            flag = false
          if (flag) {
            last = sum(j - 1)
            j += i
          }
        }
        if (j > N)
          return N / i
      }
    }
    last = D
    while (N % last != 0)
      last += 1
    N / last
  }
}