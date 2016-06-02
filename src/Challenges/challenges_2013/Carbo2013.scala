//A prefix of a string S is any leading contiguous part of S. For example, "c" and "cod" are prefixes of the string "codility". For simplicity, we require prefixes to be non-empty.
//The product of prefix P of string S is the number of occurrences of P multiplied by the length of P. More precisely, if prefix P consists of K characters and P occurs exactly T times in S, then the product equals K * T.
//For example, S = "abababa" has the following prefixes:
//"a", whose product equals 1 * 4 = 4,
//"ab", whose product equals 2 * 3 = 6,
//"aba", whose product equals 3 * 3 = 9,
//"abab", whose product equals 4 * 2 = 8,
//"ababa", whose product equals 5 * 2 = 10,
//"ababab", whose product equals 6 * 1 = 6,
//"abababa", whose product equals 7 * 1 = 7.
//The longest prefix is identical to the original string. The goal is to choose such a prefix as maximizes the value of the product. In above example the maximal product is 10.
//In this problem we consider only strings that consist of lower-case English letters (a−z).
//Write a function
//object Solution { def solution(S: String): Int }
//that, given a string S consisting of N characters, returns the maximal product of any prefix of the given string. If the product is greater than 1,000,000,000 the function should return 1,000,000,000.
//For example, for a string:
//S = "abababa" the function should return 10, as explained above,
//S = "aaa" the function should return 4, as the product of the prefix "aa" is maximal.
//Assume that:
//N is an integer within the range [1..300,000];
//string S consists only of lower-case letters (a−z).
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N) (not counting the storage required for input arguments).

package Challenges.challenges_2013

import scala.math._

//p[x]表示s[x..N - 1]这个子串和s本身的最长公共前缀的长度。我们不定义p[0],所以x > 0。我们试图线性时间计算p。
//假设我们已经计算出p[1],p[2],p[i - 1]，我们定义 right = max{p[y] + y - 1}，
//即right是前面求得所有前缀的最右边界，left是right取得最大值的y。
//也就是说s[left..right]和s[0..right - left + 1]是一样的。初始定义left = right = -1。
//(1) 如果我们发现，right >=i， 这说明 i被包含在窗口内，于是我们发现s[i..right]和s[i'..right - left + 1]是一样的，其中i' = i - left。而p[i']已经计算过了。
//（1a) 如果p[i'] < right - i + 1, 这说明这个字符串完全被窗口限定住了，我们立刻得到p[i] = p[i']。
//  (1b) 如果p[i'] >= right - i + 1,这时所得到的前缀可能更长，我们已经至少有p[i] >= p[i']了。
//我们继续比较(right + 1)和（i' + p[i']) …… 直到不match，然后更新right和left 。
//（2） 如果我们发现right < i， 那么只好沿着i暴力比较前缀，可能的话，更新left和right。

object Carbo2013 {
  def main(args: Array[String]) {
    val S = "abababa"
    println(solution(S))
  }
  //k为本次匹配的起始位置，a为到达最远匹配位置的起始匹配点，即left
  def solution(S: String): Int = {
    val N = S.length()
    var num = new Array[Int](N + 1)
    var k = 1
    var p = 0
    var q = -1
    var a = 0
    var next = new Array[Int](N)
    next(0) = N
    num(N) = 1
    while (k < N) {
      if (q < 0 || k + next(k - a) >= p) {
        if (q < 0) {
          q = 0
          p = k
        }
        while (p < N && S(p) == S(q)) {
          p += 1
          q += 1
        }
        next(k) = q
        a = k
      } else {
        next(k) = next(k - a)
      }
      num(next(k)) += 1
      k += 1
      q -= 1
    }
    var total = 0
    var ans = 0
    for (i <- N to (1, -1)) {
      total += num(i)
      if (total > 1000000000 / i)
        return 1000000000
      ans = max(ans, total * i)
    }
    ans
  }
}