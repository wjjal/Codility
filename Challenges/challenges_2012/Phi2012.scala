//A board consisting of squares arranged into N rows and M columns is given. A tiling of this board is a pattern of tiles that covers it. A tiling is interesting if:
//only tiles of size 1x1 and/or 2x2 are used;
//each tile of size 1x1 covers exactly one whole square;
//each tile of size 2x2 covers exactly four whole squares;
//each square of the board is covered by exactly one tile.
//For example, the following images show a few interesting tilings of a board of size 4 rows and 3 columns:
//
//Two interesting tilings of a board are different if there exists at least one square on the board that is covered with a tile of size 1x1 in one tiling and with a tile of size 2x2 in the other. For example, all tilings shown in the images above are different.
//Write a function:
//object Solution { def solution(N: Int, M: Int): Int }
//that, given two integers N and M, returns the remainder modulo 10,000,007 of the number of different interesting tilings of a board of size N rows and M columns.
//For example, given N = 4 and M = 3, the function should return 11, because there are 11 different interesting tilings of a board of size 4 rows and 3 columns:
//
//Assume that:
//N is an integer within the range [1..1,000,000];
//M is an integer within the range [1..7].
//Complexity:
//expected worst-case time complexity is O(log(N)*8M);
//expected worst-case space complexity is O(4M).

package challenges_2012

object Phi2012 {
  def main(args: Array[String]) {
    val N = 4
    val M = 2
    println(solution(N, M))
  }

  val MOD = 10000007
  def solution(N: Int, M: Int): Int = {
    var row = N
    var total = 1 << M
    //行表示一个状态,列表示一个状态 ,(x,y)表示上一行是状态x的时候 ,有多少种方法可以把上一行填满 ,并且这一行变为状态y
    var a = Array.ofDim[Int](total, total)
    var r = Array.ofDim[Int](total, total)
    for (i <- 0 until total) {
      count(a, 0, M, i, 0)
      r(i)(i) = 1
    }
     for (i <- 0 until total) {
        for (j <- 0 until total) {
          print(a(i)(j) + " ")
        }
        println()
      }

    while (row > 0) {
      if ((row & 1) == 1)
        r = mulmatrix(r, a)
      a = mulmatrix(a, a)
      row >>= 1
    }
    return r(0)(0)
  }

  def count(a: Array[Array[Int]], col: Int, n: Int, last: Int, now: Int) {
    if (col >= n) {
      a(last)(now) += 1
      return
    }
    //不填，或者用1*1填
    count(a, col + 1, n, last, now)
    if ((last & (1 << col)) == 0 && (col + 1 < n) && (last & (1 << (col + 1))) == 0)
      count(a, col + 2, n, last, now | (3 << col))
  }

  def mulmatrix(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    var n = a.length
    var c = Array.ofDim[Int](n, n)
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        for (k <- 0 until n) {
          c(i)(j) = add(c(i)(j), mul(a(i)(k), b(k)(j)))
        }
      }
    }
    c
  }

  def add(x: Int, y: Int): Int = {
    var re = x + y
    if (re >= MOD)
      re - MOD
    else
      re
  }

  def mul(x: Long, y: Long): Int = {
    (x * y % MOD).toInt
  }
}