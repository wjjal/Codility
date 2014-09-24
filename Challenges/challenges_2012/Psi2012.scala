//There is an N × N square mesh-shaped grid of wires, as shown in a figure below. Nodes of the grid are at points (X, Y), where X and Y are integers from 0 to N−1. An electric current flows through the grid, between the nodes at (0, 0) and (N−1, N−1).
//
//Initially, all the wires conduct the current, but the wires burn out at a rate of one per second. The burnouts are described by three zero-indexed arrays of integers, A, B and C, each of size M. For each moment T (0 ≤ T < M), in the T-th second the wire between nodes (A[T], B[T]) and:
//(A[T], B[T] + 1), if C[T] = 0 or
//(A[T] + 1, B[T]), if C[T] = 1
//burns out. You can assume that the arrays describe existing wires, and that no wire burns out more than once. Your task is to determine when the current stops flowing between the nodes at (0,0) and (N−1,N−1).
//Write a function:
//object Solution { def solution(N: Int, A: Array[Int], B: Array[Int], C: Array[Int]): Int }
//that, given integer N and arrays A, B and C, returns the number of seconds after which the current stops flowing between the nodes at (0, 0) and (N−1, N−1). If the current keeps flowing even after all M wires burn out, the function should return −1.
//For example, given N = 4, M = 9 and the following arrays:
//  A[0] = 0    B [0] = 0    C[0] = 0  
//  A[1] = 1    B [1] = 1    C[1] = 1  
//  A[2] = 1    B [2] = 1    C[2] = 0  
//  A[3] = 2    B [3] = 1    C[3] = 0  
//  A[4] = 3    B [4] = 2    C[4] = 0  
//  A[5] = 2    B [5] = 2    C[5] = 1  
//  A[6] = 1    B [6] = 3    C[6] = 1  
//  A[7] = 0    B [7] = 1    C[7] = 0  
//  A[8] = 0    B [8] = 0    C[8] = 1  
//your function should return 8, because just after the eighth wire burns out, there is no connection between the nodes at (0, 0) and (N−1, N−1). This situation is shown in the following figure:
//
//Given N = 4, M = 1 and the following arrays:
//  A[0] = 0    B [0] = 0    C[0] = 0  
//your function should return −1, because burning out a single wire cannot break the connection between the nodes at (0, 0) and (N−1, N−1).
//Assume that:
//N is an integer within the range [1..400];
//M is an integer within the range [0..2*N*(N−1)];
//each element of arrays A, B is an integer within the range [0..N−1];
//each element of array C is an integer that can have one of the following values: 0, 1.
//Complexity:
//expected worst-case time complexity is O(N2*log(N));
//expected worst-case space complexity is O(N2), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges_2012

import scala.collection.mutable.HashSet

object Psi2012 {
  def main(args: Array[String]) {
    var N = 4
    var A = Array(0, 1, 1, 2, 3, 2, 1, 0, 0)
    var B = Array(0, 1, 1, 1, 2, 2, 3, 1, 0)
    var C = Array(0, 1, 0, 0, 0, 1, 1, 0, 1)
    println(solution(N, A, B, C))
  }

  def solution(N: Int, A: Array[Int], B: Array[Int], C: Array[Int]): Int = {
    var X = A.length
    if (X < N + N - 2)
      return -1
    var grid = new Grid(N)
    var removed = new HashSet[(Int, Int, Int, Int)]()
    for (i <- 0 until X) {
      if (C(i) == 0)
        removed += ((A(i), B(i), A(i), B(i) + 1))
      else
        removed += ((A(i), B(i), A(i) + 1, B(i)))
    }
    for (i <- 0 until N) {
      for (j <- 0 until N) {
        if (j + 1 < N && !removed.contains((i, j, i, j + 1)))
          grid.union(i, j, i, j + 1)
        if (i + 1 < N && !removed.contains((i, j, i + 1, j)))
          grid.union(i, j, i + 1, j)
      }
    }
    if (grid.isConnected(0, 0, N - 1, N - 1))
      return -1
    for (i <- X - 1 to (0, -1)) {
      if (C(i) == 0)
        grid.union(A(i), B(i), A(i), B(i) + 1)
      else
        grid.union(A(i), B(i), A(i) + 1, B(i))
      if (grid.isConnected(0, 0, N - 1, N - 1))
        return i + 1
    }
    -1
  }

  class WeightedQuickUnion(size: Int) {
    assert(size > 0)
    var id = new Array[Int](size)
    var weight = new Array[Int](size)
    for (i <- 0 until size) {
      id(i) = i
      weight(i) = 1
    }

    def findRoot(child: Int): Int = {
      assert(child >= 0 && child < id.length)
      if (child != id(child))
        id(child) = findRoot(id(child))
      id(child)
    }

    def isConnected(p: Int, q: Int): Boolean = {
      assert(p >= 0 && p < id.length && q >= 0 && q < id.length)
      findRoot(p) == findRoot(q)
    }

    def union(p: Int, q: Int) {
      assert(p >= 0 && p < id.length && q >= 0 && q < id.length)
      var rootP = findRoot(p)
      var rootQ = findRoot(q)
      if (rootP == rootQ)
        return
      if (weight(rootP) >= weight(rootQ)) {
        id(rootQ) = rootP
        weight(rootP) += weight(rootQ)
      } else {
        id(rootP) = rootQ
        weight(rootQ) += weight(rootP)
      }
    }
  }

  class Grid(size: Int) {
    var grid = new WeightedQuickUnion(size * size)

    def isConnected(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
      var p = x1 * size + y1
      var q = x2 * size + y2
      grid.isConnected(p, q)
    }

    def union(x1: Int, y1: Int, x2: Int, y2: Int) {
      var p = x1 * size + y1
      var q = x2 * size + y2
      grid.union(p, q)
    }
  }
}