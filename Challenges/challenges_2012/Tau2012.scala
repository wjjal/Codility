//Neverland has very interesting topography. It is a flat land, but it seems to be the surface of a torus. That is, its map is a rectangle, but its northern edge is adjacent to its southern edge and, at the same time, its western edge is adjacent to its eastern edge. That is why it is so difficult to escape from Neverland...
//Recently, the land in Neverland has gone up for sale. It has been divided into a grid of unit squares, comprising M rows and N columns. Rows are numbered on the map from 0 to M − 1, from north to south, and columns are numbered on the map from 0 to N − 1, from west to east. Row M − 1 is adjacent to row 0 and column N − 1 is adjacent to column 0.
//You are allowed to buy one rectangular lot, which you plan to divide into smaller lots and sell for a profit. Note that your lot can overlap the edges of the map.
//A zero-indexed matrix C of integers, consisting of M rows and N columns, is given. C[I][J] equals the expected profit that you can make on the unit square in row I and column J.
//Write a function:
//object Solution { def solution(C: Array[Array[Int]]): Int }
//that, given such a matrix C, returns the maximum possible profit that you can make. If there is no profitable lot for you to buy, the function should return 0.
//For example, consider the following matrix C, consisting of three rows and three columns:
//  C[0][0] = 1    C[0][1] = -1   C[0][2] = 2
//  C[1][0] = -1   C[1][1] = -1   C[1][2] = -1
//  C[2][0] = 3    C[2][1] = -1   C[2][2] = 4
//The unit squares in row 1 and column 1 are not profitable. Only the unit squares in the corners of the map are profitable, and they form a 2 × 2 square. So, this 2 × 2 square is the optimal lot, and the profit it can earn for you is 10.
//Now consider the following matrix C, consisting of two rows and two columns:
//  C[0][0] = 1     C[0][1] = -3
//  C[1][0] = -2    C[1][1] = 3
//In this example there are only two single unit square lots that are profitable, and the maximum profit is 3.
//Assume that:
//M and N are integers within the range [1..100];
//each element of matrix C is an integer within the range [−10,000..10,000].
//Complexity:
//expected worst-case time complexity is O((M+N)3);
//expected worst-case space complexity is O(M*N).

package challenges_2012

import scala.math._

object Tau2012 {
  def solution(C: Array[Array[Int]]): Int = {
    val M = C.length
    val N = C(0).length
    var a = new Array[Array[Int]](M << 1)
    a(0) = new Array[Int](N)
    for (i <- 1 to M) {
      a(i) = new Array[Int](N)
      for (j <- 0 until N) {
        a(i)(j) = a(i - 1)(j) + C(i - 1)(j)
      }
    }
    for (i <- 1 until M) {
      a(i + M) = new Array[Int](N)
      for (j <- 0 until N) {
        a(i + M)(j) = a(i + M - 1)(j) + C(i - 1)(j)
      }
    }
    var ans = 0
    var b = new Array[Int](N)
    for (i <- 1 to M) { //number of row to choose
      for (j <- 1 to M) { //begin of row
        for (k <- 0 until N) {
          b(k) = a(j + i - 1)(k) - a(j - 1)(k)
        }
        ans = max(ans, maximum(b))
        var s = 0
        for (k <- 0 until N) {
          s += b(k)
          b(k) = -b(k)
        }
        ans = max(ans, s + maximum(b))
      }
    }
    ans
  }

  def maximum(a: Array[Int]): Int = {
    var sum = 0
    var max = 0
    for (i <- 0 until a.length) {
      if (sum < 0)
        sum = 0
      sum += a(i)
      if (sum > max)
        max = sum
    }
    max
  }
}