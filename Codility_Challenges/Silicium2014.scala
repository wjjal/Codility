//You are presented with a rectangular cake whose sides are of length X and Y. The cake has been cut into (N + 1)2 pieces by making N straight cuts along the first side and N straight cuts along the second side.
//The cuts are represented by two non-empty zero-indexed arrays A and B consisting of N integers. More precisely, A[I] such that 0 ≤ I < N represents the position of a cut along the first side, and B[I] such that 0 ≤ I < N represents the position of a cut along the second side.
//The goal is to find the K-th piece of cake in order of size, starting with the largest piece first. We will consider the size of a piece to be its area.
//For example, a cake with sides X = 6, Y = 7 and arrays A and B such that:
//    A[0] = 1    B[0] = 1
//    A[1] = 3    B[1] = 5
//is represented by the figure below.
//
//There are nine pieces of cake, and their consecutive sizes are: 12, 8, 6, 4, 4, 3, 2, 2, 1. In the figure above, the third piece of cake is highlighted; its size equals 6.
//Write a function:
//object Solution { def solution(X: Int, Y: Int, K: Int, A: Array[Int], B: Array[Int]): Int }
//that, given three integers X, Y, K and two non-empty zero-indexed arrays A and B of N integers, returns the size of the K-th piece of cake.
//For example, given:
//    X = 6   Y = 7   K = 3
//    A[0] = 1    B[0] = 1
//    A[1] = 3    B[1] = 5
//the function should return 6, as explained above.
//Assume that:
//N is an integer within the range [1..40,000];
//X and Y are integers within the range [2..400,000,000];
//K is an integer within the range [1..(N+1)*(N+1)];
//each element of array A is an integer within the range [1..X−1];
//each element of array B is an integer within the range [1..Y−1];
//A[I − 1] < A[I] and B[I − 1] < B[I], for every I such that 0 < I < N;
//1 ≤ A[I] − A[I − 1], B[I] − B[I − 1] ≤ 10,000, for every I such that 0 < I < N.
//Complexity:
//expected worst-case time complexity is O(N*log(N+X+Y));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges

object Silicium2014 {
  def main(args: Array[String]) {
    val X = 6
    val Y = 7
    val K = 3
    val A = Array(1, 3)
    val B = Array(1, 5)
    println(solution(X, Y, K, A, B))
  }
  def solution(X: Int, Y: Int, K: Int, A: Array[Int], B: Array[Int]): Int = {
    val N = A.length + 1
    var xs = new Array[Int](N)
    var ys = new Array[Int](N)
    xs(0) = A(0)
    for (i <- 1 until N - 1)
      xs(i) = A(i) - A(i - 1)
    xs(A.length) = X - A(N - 2)
    ys(0) = B(0)
    for (i <- 1 until N - 1)
      ys(i) = B(i) - B(i - 1)
    ys(B.length) = Y - B(N - 2)
    xs = xs.sorted
    ys = ys.sorted
    binarySearch(xs, ys, K)
  }

  def binarySearch(a: Array[Int], b: Array[Int], K: Int): Int = {
    val N = a.length
    var begin = 0
    var end = a(N - 1) * b(N - 1)
    while (begin <= end) {
      var mid = (begin + end) >> 1
      var tmp = cal(mid, a, b)
      if (tmp >= K)
        begin = mid + 1
      else
        end = mid - 1
    }
    end + 1
  }

  def cal(num: Int, a: Array[Int], b: Array[Int]): Int = {
    var N = a.length
    if (a(N - 1) * b(N - 1) <= num)
      return 0
    var j = N - 1
    var ans = 0
    for (i <- 0 until N) {
      while (j >= 0 && a(i) * b(j) > num)
        j -= 1
      ans += N - 1 - j
    }
    ans
  }
}