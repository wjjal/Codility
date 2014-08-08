//In this problem we consider only strings consisting of lower-case English letters (a−z).
//A string is a palindrome if it reads exactly the same from left to right as it does from right to left. For example, these strings are palindromes:
//aza
//abba
//abacaba
//These strings are not palindromes:
//zaza
//abcd
//abacada
//Given a string S of length N, a slice of S is a substring of S specified by a pair of integers (p, q), such that 0 ≤ p < q < N. A slice (p, q) of string S is palindromic if the string consisting of letters S[p], S[p+1], ..., S[q] is a palindrome. For example, in a string S = abbacada:
//slice (0, 3) is palindromic because abba is a palindrome,
//slice (6, 7) is not palindromic because da is not a palindrome,
//slice (2, 5) is not palindromic because baca is not a palindrome,
//slice (1, 2) is palindromic because bb is a palindrome.
//Write a function
//object Solution { def solution(S: String): Int }
//that, given a string S of length N letters, returns the number of palindromic slices of S. The function should return −1 if this number is greater than 100,000,000.
//For example, for string S = baababa the function should return 6, because exactly six of its slices are palindromic; namely: (0, 3), (1, 2), (2, 4), (2, 6), (3, 5), (4, 6).
//Assume that:
//N is an integer within the range [0..20,000];
//string S consists only of lower-case letters (a−z).
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N) (not counting the storage required for input arguments).

package challenges

import scala.math._

object Gamma2011 {
  def main(args: Array[String]) {
    val S = "baababa"
    println(solution(S))
  }

  def solution(S: String): Int = {
    val N = S.length()
    if (S.isEmpty())
      return 0
    var s = new Array[Char](N * 2 + 1)
    for (i <- 0 until N) {
      s(2 * i) = '#'
      s(2 * i + 1) = S(i)
    }
    s(2 * N) = '#'
    val n = s.length()
    var palindrome_len = new Array[Int](n)
    var bound = 0
    var center = 0
    for (i <- 0 until n) {
      if (bound > i)
        palindrome_len(i) = min(palindrome_len(2 * center - i), bound - i)
      else
        palindrome_len(i) = 1
      while (i - palindrome_len(i) >= 0 && i + palindrome_len(i) < n && s(i - palindrome_len(i)) == s(i + palindrome_len(i)))
        palindrome_len(i) += 1
      if (bound < palindrome_len(i) + i) {
        center = i
        bound = palindrome_len(i) + i
      }
    }
    var ans = 0
    ans = (palindrome_len.filter(_ > 2).map(x => (x - 1) / 2)).sum
    if (ans >= 100000000)
      return -1
    ans
  }
}