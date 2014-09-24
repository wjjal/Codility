//There are N+1 towns numbered from 0 to N lying along a highway. The distances between towns, and gas prices in each town, are given. A truck has to deliver cargo from town 0 to town N. What is the cheapest way to buy enough gas for this trip?
//Two non-empty zero-indexed arrays D (distances) and P (prices), each consisting of N positive integers, and a positive integer T (tank capacity) are given. Consider any zero-indexed sequence R (refill strategy) consisting of N non-negative integers (amount of fuel bought in each town). 
//Sequence L (amount of fuel when leaving town), consisting of N integers, is defined as follows: 
//L[K] = (R[0] + ... + R[K]) − (D[0] + ... + D[K−1]) for 0 ≤ K ≤ N−1 
//Sequence A (amount of fuel when arriving at town), consisting of N integers, is defined as follows: 
//A[K] = L[K−1] − D[K−1] for 1 ≤ K ≤ N 
//The following conditions (meaning that the truck must not run out of fuel and cannot fill up with more fuel than its tank capacity) must hold: 
//0 ≤ L[K] ≤ T for 0 ≤ K ≤ N−1 
//0 ≤ A[K] ≤ T for 1 ≤ K ≤ N 
//Number C (total cost of refill strategy) is defined as: 
//C = R[0] * P[0] + ... + R[N−1] * P[N−1]
//For example, consider T = 15 and the following arrays D and P, consisting of three elements each:
//  D[0] = 10    D[1] = 9    D[2] = 8
//  P[0] =  2    P[1] = 1    P[2] = 3
//Sequence [15, 10, 2] is a refill strategy with cost 46. Sequence [10, 5, 12] is not a valid refill strategy, because the truck would run out of fuel between towns 1 and 2. Sequence [10, 15, 2] is a refill strategy with cost 41, and no cheaper refill strategy exists for this choice of arrays D, P and number T.
//Write a function:
//object Solution { def solution(D: Array[Int], P: Array[Int], T: Int): Int }
//that, given two non-empty zero-indexed arrays D and P consisting of N positive integers each and a positive integer T, returns the cost of the cheapest refill strategy for D, P and T. 
//The function should return −1 if no valid refill strategy exists. 
//The function should return −2 if the result exceeds 1,000,000,000.
//For example, given T = 15 and arrays D and P consisting of three elements each such that:
//  D[0] = 10    D[1] = 9    D[2] = 8
//  P[0] =  2    P[1] = 1    P[2] = 3
//the function should return 41, as explained above.
//Assume that:
//T is an integer within the range [1..1,000,000];
//N is an integer within the range [1..100,000];
//each element of arrays D, P is an integer within the range [1..1,000,000].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges_2011

import scala.collection.mutable.DoubleLinkedList

object Theta2011 {
  def main(args: Array[String]) {
    val D = Array(10, 10, 10)
    val P = Array(20, 10, 5)
    val T = 10
    println(solution(D, P, T))
  }
  def solution(D: Array[Int], P: Array[Int], T: Int): Int = {
    val N = D.length
    var have = new DoubleLinkedList[(Int, Int)]()
    var cost = 0
    have = have.:+(P(0), T)
    for (i <- 0 until N)
      if (D(i) > T)
        return -1
    for (i <- 1 to N) {
      var input = D(i - 1)
      var need = D(i - 1)
      while (!have.isEmpty && have.head._2 <= need) {
        need -= have.head._2
        if ((1000000000 - cost) / have.head._1 < have.head._2)
          return -2
        cost += have.head._1 * have.head._2
        have = have.tail
      }
      if (need != 0) {
        if ((1000000000 - cost) / have.head._1 < need)
          return -2
        cost += have.head._1 * need
        var temp = have.head
        have = have.tail
        have = (temp._1, temp._2 - need) +: have
      }
      if (i == N)
        return cost
      while (!have.isEmpty && P(i) < have.last._1) {
        input += have.last._2
        have = have.dropRight(1)
      }
      have = have.:+(P(i), input)
    }
    cost
  }
}