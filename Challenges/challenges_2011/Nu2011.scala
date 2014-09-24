//The median of a sequence of numbers X[0], X[1], ..., X[N] is the middle element in terms of their values. More formally, the median of X[0], X[1], ..., X[N] is an element X[I] of the sequence, such that at most half of the elements are larger than X[I] and at most half of the elements are smaller than X[I]. For example, the median of the following sequence:
//  X[0] = 7    X[1] = 2    X[2] = 5    X[3] = 2    X[4] = 8    
//is 5; the median of the following sequence:
//  X[0] = 2    X[1] = 2    X[2] = 5    X[3] = 2       
//is 2; and the following sequence:
//  X[0] = 1    X[1] = 5    X[2] = 7    
//  X[3] = 4    X[4] = 2    X[5] = 8    
//has two medians: 4 and 5.
//Note that sequences of odd length have only one median, which is equal to X[N/2] after sorting X. In this problem we consider medians of sequences of odd length only.
//Write a function:
//object Solution { def solution(A: Array[Int], B: Array[Int], P: Array[Int], Q: Array[Int], R: Array[Int], S: Array[Int]): Int }
//that, given:
//two non-empty zero-indexed arrays, A (consisting of N integers) and B (consisting of M integers), both sorted in ascending order
//two zero-indexed arrays P and Q, each consisting of K indices of array A, such that 0 ≤ P[I] ≤ Q[I] < N for 0 ≤ I < K
//two zero-indexed arrays R and S, each consisting of K indices of array B, such that 0 ≤ R[I] ≤ S[I] < M for 0 ≤ I < K
//computes medians of K sequences of the form:
//  A[P[I]], A[P[I]+1], ..., A[Q[I]-1], A[Q[I]], B[R[I]], B[R[I]+1], ..., B[S[I]-1], B[S[I]]
//for 0 ≤ I < K, and returns the median of all such medians.
//For example, given the following arrays:
//  A[0] = -2   A[1] = 4    A[2] = 10   A[3] = 13   
//  B[0] = 5    B[1] = 6    B[2] = 8    B[3] = 12    B[4] = 13
//  P[0] = 2    P[1] = 1    P[2] = 0
//  Q[0] = 3    Q[1] = 2    Q[2] = 3
//  R[0] = 0    R[1] = 0    R[2] = 1
//  S[0] = 4    S[1] = 0    S[2] = 3
//the function should return 8, since:
//the median of [10, 13, 5, 6, 8, 12, 13] equals 10,
//the median of [4, 10, 5] equals 5,
//the median of [−2, 4, 10, 13, 6, 8, 12] equals 8, and
//the median of [10, 5, 8] equals 8.
//Assume that:
//N and M are integers within the range [1..100,000];
//K is an integer within the range [1..10,000];
//each element of arrays A, B is an integer within the range [−1,000,000,000..1,000,000,000];
//array A is sorted in non-decreasing order;
//array B is sorted in non-decreasing order;
//each element of arrays P, Q is an integer within the range [0..N−1];
//each element of arrays R, S is an integer within the range [0..M−1];
//P[i] ≤ Q[i] and R[i] ≤ S[i] for 0 ≤ i < K;
//K is odd and so is Q[i]−P[i]+R[i]−S[i] for 0 ≤ i < K.
//Complexity:
//expected worst-case time complexity is O(K*log(K+N+M));
//expected worst-case space complexity is O(K), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges_2011

object Nu2011 {
  def solution(A: Array[Int], B: Array[Int], P: Array[Int], Q: Array[Int], R: Array[Int], S: Array[Int]): Int = {
    val K = P.length
    var c = new Array[Int](K)
    for (i <- 0 until K) {
      c(i) = cal(A, B, P(i), R(i), Q(i) - P(i) + 1, S(i) - R(i) + 1, (Q(i) - P(i) + 1 + S(i) - R(i) + 1) / 2 + 1)
    }
    findk(c, K, K / 2)
  }

  def cal(a: Array[Int], b: Array[Int], begina: Int, beginb: Int, numa: Int, numb: Int, k: Int): Int = {
    if (numa == 0)
      return b(beginb + k - 1)
    if (numb == 0)
      return a(begina + k - 1)
    if (k == 1) {
      if (a(begina) < b(beginb))
        return a(begina)
      else
        return b(beginb)
    }
    if (k == numa + numb) {
      if (a(begina + numa - 1) > b(beginb + numb - 1))
        return a(begina + numa - 1)
      else
        return b(beginb + numb - 1)
    }
    var ka = (k * 1.0 * numa / (numa + numb)).toInt
    if (ka == 0)
      ka = 1
    var kb = k - ka
    if (kb > numb) {
      kb = numb
      ka = k - kb
    }
    if (a(begina + ka - 1) <= b(beginb + kb - 1))
      cal(a, b, begina + ka, beginb, numa - ka, kb, k - ka)
    else
      cal(a, b, begina, beginb + kb, ka, numb - kb, k - kb)
  }

  def findk(a: Array[Int], n: Int, k: Int): Int = {
    var b = a.sorted
    b(k)
  }
}