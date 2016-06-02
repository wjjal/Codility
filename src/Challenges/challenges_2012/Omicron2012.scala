//The Fibonacci sequence is defined by the following recursive formula:
//F(0) = 0
//F(1) = 1
//F(N) = F(N−1) + F(N−2) for N ≥ 2
//Write a function:
//object Solution { def solution(N: Int, M: Int): Int }
//that, given two non-negative integers N and M, returns a remainder of F(NM) modulo 10,000,103.
//Note: 10,000,103 is a prime number.
//For example, given N = 2 and M = 3, the function should return 21, since 23 = 8 and F(8) = 21.
//Assume that:
//N and M are integers within the range [0..10,000,000].
//Complexity:
//expected worst-case time complexity is O(log(N+M));
//expected worst-case space complexity is O(1).

package Challenges.challenges_2012

object Omicron2012 {
  val mod = 10000103
  val F = Array(Array(1L, 1L), Array(1L, 0L))
  val id = Array(Array(1L, 0L), Array(0L, 1L))

  def main(args: Array[String]) {
    val N = 10
    val M = 2
    println(solution(N, M))
  }

  def solution(N: Int, M: Int): Int = {
    val Q = count_period()
    fib(exp(N, M, Q))
  }

  def count_period(): Int = {
    var a = 1
    var b = 2
    var i = 1
    var c = a + b
    while ((a != b) || (a != 1)) {
      c = (a + b) % mod
      a = b
      b = c
      i += 1
    }
    i
  }

  def exp(N: Long, M: Long, Q: Int): Long = {
    if (M == 0)
      1
    else if (M % 2 == 0)
      exp((N * N) % Q, M / 2, Q)
    else
      N * exp(N, M - 1, Q) % Q
  }

  def fib(K: Long): Int = {
    var A = pow(F, K)
    A(0)(1).toInt
  }

  def pow(A: Array[Array[Long]], K: Long): Array[Array[Long]] = {
    if (K == 0)
      id
    else if (K % 2 == 0)
      pow(mul(A, A), K / 2)
    else
      mul(pow(A, K - 1), A)
  }

  def mul(A: Array[Array[Long]], B: Array[Array[Long]]): Array[Array[Long]] = {
    var C = new Array[Array[Long]](2)
    C(0) = new Array[Long](2)
    C(1) = new Array[Long](2)
    C(0)(0) = (A(0)(0) * B(0)(0) % mod + A(0)(1) * B(1)(0) % mod) % mod
    C(0)(1) = (A(0)(0) * B(0)(1) % mod + A(0)(1) * B(1)(1) % mod) % mod
    C(1)(0) = (A(1)(0) * B(0)(0) % mod + A(1)(1) * B(1)(0) % mod) % mod
    C(1)(1) = (A(1)(0) * B(0)(1) % mod + A(1)(1) * B(1)(1) % mod) % mod
    C
  }
}