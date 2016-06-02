//In the binary number "100100010000" there are at least two 0s between any two consecutive 1s.
//In the binary number "100010000100010" there are at least three 0s between any two consecutive 1s.
//A positive integer N is called K-sparse if there are at least K 0s between any two consecutive 1s in its binary representation.
//For example, the binary number "100100010000" is 2-sparse. Similarly, "100010000100010" is 3-sparse. It is also 2-sparse, because 2 < 3. It is also 1-sparse and 0-sparse.
//We assume that any power of 2 (i.e. "1", "10", "100", "1000", ...) is K-sparse for any K.
//Write a function:
//object Solution { def solution(S: String, T: String, K: Int): Int }
//that, given:
//string S containing a binary representation of some positive integer A,
//string T containing a binary representation of some positive integer B,
//a positive integer K.
//returns the number of K-sparse integers within the range [A..B] (both ends included). If the result exceeds 1,000,000,006, the function should return the remainder from the division of the result by 1,000,000,007.
//For example, given S = "101" (A = 5), T = "1111" (B=15) and K=2, the function should return 2, because there are just two 2-sparse integers in the range [5..15], namely "1000" (i.e. 8) and "1001" (i.e. 9).
//Assume that:
//K is an integer within the range [1..30];
//the length of S is within the range [1..300,000];
//the length of T is within the range [1..300,000];
//string S consists only of the characters "0" and/or "1";
//string T consists only of the characters "0" and/or "1";
//S and T have no leading zeros;
//A ≤ B.
//Complexity:
//expected worst-case time complexity is O(log(B));
//expected worst-case space complexity is O(log(B)) (not counting the storage required for input arguments).
//Notation used:
//B − number represented by T.

//设dp[n]表示n位满足要求的2进制数（允许首位是0）的个数。
//规定dp[0] = 1, dp[1] = 2。dp[1] = 2是只有1位时，它为0或者为1都可以。
//那么我们考虑dp[n] (n > 1)，如果它首位为0，则它的后面(n - 1)位一定满足条件。
//如果它首位为1，则它后必须有K个0，再接一个有效的数即dp[n] = dp[n - 1] + dp[n - k - 1]。
//这里要注意当n - k - 1 < 0时，我们要加1，也就是初始条件对于dp[x] = 1 if x <= 0。
//这里的原因是这样的，当首位为1时，后面位数不足K位时，全为0式满足条件的。
//于是统一一下，方程为dp[n] = dp[n - 1] + dp[n - k - 1] 初始条件是 dp[x] = 1 if x <= 0。

package Challenges.challenges_2012

object Xi2012 {
  def main(args: Array[String]) {
    var S = "1001001"
    var T = "10010011"
    var K = 1
    println(solution(S, T, K))
  }

  val M = 1000000007
  def solution(S: String, T: String, K: Int): Int = {
    val N = T.length()
    var dp = new Array[Int](N + 1)
    dp(0) = 1
    for (i <- 1 to N) {
      if (i <= K)
        dp(i) = add(dp(i - 1),1)
      else
        dp(i) = add(dp(i - 1), dp(i - K - 1))
    }
    var a = dec(S)
    var i = cal(T, K, dp) - cal(a, K, dp)
    if (i < 0)
      i + M
    else
      i
  }

  def cal(s: String, k: Int, dp: Array[Int]): Int = {
    var last = -1
    var ans = 0
    var n = s.length()
    var vaild = true
    for (i <- 0 until n) {
      if (s(i) == '1') {
        if (last < 0) {
          ans = add(ans, dp(n - 1 - i))
        } else {
          if (i - last <= k)
            vaild = false
          if (i - last >= k)
            ans = add(ans, dp(n - 1 - i))
          else if (last + k >= n)
            ans = add(ans, 1)
          else
            ans = add(ans, dp(n - 1 - last - k))
        }
        last = i
      }
      if (!vaild)
        return ans
    }
    if (vaild)
      ans = add(ans, 1)
    ans
  }

  def dec(s: String): String = {
    var re = s
    for (i <- s.length() - 1 to (0, -1)) {
      if (re(i) == '0')
        re =re.updated(i, '1')
      else {
        re = re.updated(i, '0')
        return re
      }
    }
    re
  }

  def add(x: Int, y: Int): Int = {
    var re = x + y
    if (re >= M)
      re - M
    else
      re
  }
}