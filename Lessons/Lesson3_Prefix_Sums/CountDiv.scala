//Write a function:
//object Solution { def solution(A: Int, B: Int, K: Int): Int }
//that, given three integers A, B and K, returns the number of integers within the range [A..B] that are divisible by K, i.e.:
//{ i : A ≤ i ≤ B, i mod K = 0 }
//For example, for A = 6, B = 11 and K = 2, your function should return 3, because there are three numbers divisible by 2 within the range [6..11], namely 6, 8 and 10.
//Assume that:
//A and B are integers within the range [0..2,000,000,000];
//K is an integer within the range [1..2,000,000,000];
//A ≤ B.
//Complexity:
//expected worst-case time complexity is O(1);
//expected worst-case space complexity is O(1).

package prefix_sums

object CountDiv {
  def solution(A: Int, B: Int, K: Int): Int = {
    if (A == 0) B / K + 1 else B / K - (A - 1) / K
  }
}