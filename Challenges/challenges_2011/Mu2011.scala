//A positive integer N is given. Consider the sequence of numbers [0, 1, ..., N]. What is the total number of zeros in the decimal representations of these numbers?
//N can be very large. Hence, it is given in the form of a non-empty string S of length L, containing a decimal representation of N. S contains no leading zeros.
//Write a function:
//object Solution { def solution(S: String): Int }
//that, given a string S, which is a decimal representation of some positive integer N, returns the total number of zeros in the decimal representations of numbers [0, 1, ..., N]. If the result exceeds 1,410,000,016, the function should return the remainder from the division of the result by 1,410,000,017.
//For example, for S="100" the function should return 12 and for S="219" it should return 42.
//Assume that:
//L is an integer within the range [1..10,000];
//string S consists only of digits (0−9);
//string S contains no leading zeros.
//Complexity:
//expected worst-case time complexity is O(L);
//expected worst-case space complexity is O(L) (not counting the storage required for input arguments).

package Challenges.challenges_2011

object Mu2011 {
  def main(args: Array[String]) {
    val S = "10161022382947"
    println(solution(S))
  }

  val P = 1410000017
  def solution(S: String): Int = {
    val len = S.length()
    var Z = 0
    var N = 0
    var F = 0
    for (i <- 0 until len) {
      F = (add(mul(10, F), N) - mul(Z, (9 - (S(i) - '0')))) % P
      if (F < 0)
        F += P
      if (S(i) == '0')
        Z += 1
      N = (mul(10, N) + (S(i) - '0')) % P
    }
    (1 + F) % P
  }

  def add(x: Long, y: Long): Int = {
    var re = x + y
    if (re >= P)
      (x - P + y).toInt
    else
      re.toInt
  }

  def mul(x: Long, y: Long): Int = {
    (x * y % P).toInt
  }
}