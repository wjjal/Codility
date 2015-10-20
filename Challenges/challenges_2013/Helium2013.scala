//A prefix of a string S is any leading contiguous part of S. A suffix of the string S is any trailing contiguous part of S. For example, "c" and "cod" are prefixes, and "ty" and "ity" are suffixes of the string "codility". For simplicity, we require prefixes and suffixes to be non-empty and shorter than the whole string S.
//A border of a string S is any string that is both a prefix and a suffix. For example, "cut" is a border of a string "cutletcut", and a string "barbararhubarb" has two borders: "b" and "barb".
//We are looking for such borders of S that have at least three non-overlapping occurrences; that is, for some string that occurs as a prefix, as a suffix and elsewhere in between. For example, for S = "barbararhubarb", the only such string is "b".
//In this problem we consider only strings that consist of lower-case English letters (a−z).
//Write a function:
//object Solution { def solution(S: String): Int }
//that, given a string S consisting of N characters, returns the length of its longest border that has at least three non-overlapping occurrences in the given string. If there is no such border in S, the function should return 0.
//For example, given a string S as follows:
//if S = "barbararhubarb" the function should return 1, as explained above;
//if S = "ababab" the function should return 2, as "ab" and "abab" are both borders of S, but only "ab" has three non-overlapping occurrences;
//if S = "baaab" the function should return 0, as its only border "b" occurs only twice.
//Assume that:
//N is an integer within the range [0..1,000,000];
//string S consists only of lower-case letters (a−z).
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N) (not counting the storage required for input arguments).

//p[x]表示s[x..N - 1]这个子串和s本身的最长公共前缀的长度。我们不定义p[0],所以x > 0。我们试图线性时间计算p。
//假设我们已经计算出p[1],p[2],p[i - 1]，我们定义 right = max{p[y] + y - 1}，即right是前面求得所有前缀的最右边界，left是right取得最大值的y。也就是说s[left..right]和s[0..right - left + 1]是一样的。初始定义left = right = -1。
//(1) 如果我们发现，right >=i， 这说明 i被包含在窗口内，于是我们发现s[i..right]和s[i'..right - left + 1]是一样的，其中i' = i - left。而p[i']已经计算过了。
//（1a) 如果p[i'] < right - i + 1, 这说明这个字符串完全被窗口限定住了，我们立刻得到p[i] = p[i']。
//  (1b) 如果p[i'] >= right - i + 1,这时所得到的前缀可能更长，我们已经至少有p[i] >= p[i']了。我们继续比较(right + 1)和（i' + p[i']) …… 直到不match，然后更新right和left 。
//（2） 如果我们发现right < i， 那么只好沿着i暴力比较前缀，可能的话，更新left和right。

package Challenges.challenges_2013

import scala.math._

object Helium2013 {
  def main(args: Array[String]) {
    val S = "aabaaabaaabaabbbaabaa"
    println(solution(S))
  }

  def solution(S: String): Int = {
    val N = S.length()
    if (N < 3)
      return 0
    var lcs = ext_KMP(S)
    var lcsmap = for { i <- 0 until N } yield (lcs(i), i)
    lcsmap = lcsmap.sortBy(_._1)(Ordering.Int.reverse)
    var mid = false
    var maxlen = 0
    for (i <- 1 until N) {
      var len = lcsmap(i)._1
      var begin = lcsmap(i)._2
      if (!mid || len > maxlen) {
        //有重叠
        if (begin <= len - 1)
          len = min(len, begin)
        if (begin + len - 1 >= N - len)
          len = min(len, (N - begin) / 2)
        if (len > maxlen)
          maxlen = max(maxlen, len)
        mid = true
      } else if (mid && begin + len - 1 == N - 1 && len <= maxlen)
        return len
    }
    0
  }

  def ext_KMP(S: String): Array[Int] = {
    val N = S.length()
    var lcs = new Array[Int](N)
    lcs(0) = N
    var left = -1
    var right = -1
    for (i <- 1 until N) {
      if (right < i) {
        var j = 0
        while (i + j < N && S(j) == S(i + j))
          j += 1
        if (j > 0) {
          lcs(i) = j
          left = i
          right = i + j - 1
        }
      } else if (right - i < lcs(i - left)) {
        var j = right + 1
        while (j < N && S(j) == S(j - i))
          j += 1
        lcs(i) = j - i
        left = i
        right = j - 1
      } else //right - i > lcs(i - left)
        lcs(i) = lcs(i - left)
    }
    lcs
  }
}

