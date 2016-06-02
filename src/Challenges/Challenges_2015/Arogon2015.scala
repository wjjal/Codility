//You want to spend your next vacation in Poland. Despite its not being a very big country, Poland has a highly diverse natural environment ranging from the Baltic Sea in the north to the Tatra Mountains in the south. As you enjoy swimming in the sea as well as trekking up mountains, you would like to spend some time in both of those locations. However, the weather in Poland can sometimes be very capricious, so you also need to take that into account when planning your vacation.
//  In the summer you are free for N consecutive days. You can start and finish your vacation on any of these days. You want to spend the first part of your vacation at the seaside and the remaining part in the mountains. These parts can each be of any positive length, and you want to maximize the total length of the vacation.
//  You have obtained a weather forecast for all N days when you are free. By curious coincidence, the weather on every day is expected to be either perfect for spending the day at the seaside but not in the mountains, or vice versa (perfect for trekking but not for swimming). Obviously, you want the best possible weather during each part of your vacation, so you require the weather to be perfect for swimming for more than half of the days in the first part of your vacation, and perfect for trekking for more than half of the days in the second part of your vacation.
//  The weather forecast is described in a zero-indexed array A: for each K (0 ≤ K < N), A[K] is equal to 0 if the weather during day K favors the seaside, or 1 if the weather during that day favors the mountains.
//
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given a zero-indexed array A of N integers, returns the length of the longest vacation consistent with your requirements.
//For example, consider array A such that:
//A[0] = 1
//A[1] = 1
//A[2] = 0
//A[3] = 1
//A[4] = 0
//A[5] = 0
//A[6] = 1
//A[7] = 1
//You are free for eight days. The weather during days 2, 4 and 5 will be better for swimming, and better for trekking during the remaining days. You can start your vacation on day 1, spend five days at the seaside (three days will have perfect weather, which is more than half) and then spend two days in the mountains (both days will have perfect weather). That results in a vacation length of seven days, which is the longest possible vacation that meets your criteria, so the function should return 7.
//For array A such that:
//A[0] = 1
//A[1] = 0
//there is no vacation that satisfies your requirements, so the function should return 0.
//Assume that:
//N is an integer within the range [2..100,000];
//each element of array A is an integer that can have one of the following values: 0, 1.
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Challenges.Challenges_2015

import scala.math._

object Arogon2015 {
  def main(args: Array[String]) {
    val A = Array(1, 1, 0, 1, 0, 0, 1, 1)
    println(solution(A))
  }

  def solution(A: Array[Int]): Int = {
    val N = A.length
    var dp = new Array[Int](N)
    var i = 0
    var j = N - 1
    while (i < N && A(i) == 1)
      i += 1
    while (j >= 0 && A(j) == 0)
      j -= 1
    if (i > j)
      return 0
    var minsum = -1
    dp(i) = -1
    for (k <- (i + 1 to j)) {
      if (A(k) == 0)
        dp(k) = dp(k - 1) - 1
      else
        dp(k) = dp(k - 1) + 1
      minsum = min(minsum, dp(k))
    }
    var maxsum = dp(j) - minsum;
    while (i > 0 && minsum < 0) {
      i -= 1
      minsum += 1
    }
    if (minsum == 0)
      i += 1
    while (j < N - 1 && maxsum > 0) {
      j += 1
      maxsum -= 1
    }
    if (maxsum == 0)
      j -= 1
    return j - i + 1
  }
}
