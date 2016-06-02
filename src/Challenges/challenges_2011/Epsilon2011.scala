//Two non-empty zero-indexed arrays A and B, each consisting of N integers, are given. Four functions are defined based on these arrays:
//F(X,K)	=	A[K]*X + B[K]
//U(X)	=	max{ F(X,K) : 0 ≤ K < N }
//D(X)	=	min{ F(X,K) : 0 ≤ K < N }
//S(X)	=	U(X) − D(X)
//Write a function:
//object Solution { def solution(A: Array[Int], B: Array[Int]): Double }
//that, given two arrays A and B consisting of N integers each, returns the minimum value of S(X) where X can be any real number.
//For example, given the following arrays A and B consisting of three elements each:
//A[0] = -1    B[0] = 3
//A[1] =  1    B[1] = 0
//A[2] =  0    B[2] = 2
//the function should return 0.5 because:
//U(X)		=		−1*X + 3		if		X ≤ 1
//U(X)		=		0*X + 2		if		1 ≤ X ≤ 2
//U(X)		=		1*X + 0		if		2 ≤ X
//and:
//D(X)		=		1*X + 0		if		X ≤ 1.5
//D(X)		=		−1*X + 3		if		1.5 ≤ X
//so for X = 1.5, function S(X) is equal to 0.5 and this is the minimum value of this function.
//Assume that:
//N is an integer within the range [1..100,000];
//each element of arrays A, B is an integer within the range [−1,000..1,000].
//Complexity:
//expected worst-case time complexity is O(N*log(N));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Challenges.challenges_2011

import scala.collection.mutable.ArrayBuffer

object Epsilon2011 {
  def main(args: Array[String]) {
    val A = Array(3,2,7)
    val B = Array(1,2,-2)
    println(solution(A, B))
  }

  def solution(A: Array[Int], B: Array[Int]): Double = {
    var all = new ArrayBuffer[(Int, Int)]()
    var temp = (0, 0)
    var n = A.length
    for (i <- 0 until n) {
      all.+=((A(i), B(i)))
    }
    all = all.sortBy(x => (x._1, x._2))
    //UP存储交点，U存储线段，UP(i)为U(i)和U(i+1)两条线段的交点
    var U = new ArrayBuffer[(Int, Int)]()
    var UP = new ArrayBuffer[(Int, Int)]()
    make(all, UP, U)
    for (i <- 0 until n) {
      all.update(i, (-all(i)._1, -all(i)._2))
    }
    var i = 0
    var j = n - 1
    while (i < j) {
      temp = all(i)
      all(i) = all(j)
      all(j) = temp
      i += 1
      j -= 1
    }
    var D = new ArrayBuffer[(Int, Int)]()
    var DP = new ArrayBuffer[(Int, Int)]()
    make(all, DP, D)
    var inf = true
    var r = 0D
    var may = 0D
    var c = 0
    i = 0
    j = 0
    var flag = true
    while (flag) {
      var infi = (i >= UP.size)
      var infj = (j >= DP.size)
      if (infi) {
        if (infj)
          flag = false
        else
          c = 1
      } else if (infj)
        c = -1
      else {
        c = cmp(UP(i), DP(j))
      }
      if (flag) {
        var k = (U(i)._1 + D(j)._1).toDouble
        var b = (U(i)._2 + D(j)._2).toDouble
        if (c > 0) {
          may = k * DP(j)._1 / DP(j)._2 + b
          j += 1
        } else if (c == 0) {
          may = k * DP(j)._1 / DP(j)._2 + b
          i += 1
          j += 1
        } else {
          may = k * UP(i)._1 / UP(i)._2 + b
          i += 1
        }
        if (inf || (r > may)) {
          inf = false
          r = may
        }
      }
    }
    if (inf)
      r = U(0)._2 + D(0)._2
    r
  }

  //比较两个交点a,b的大小，a:a1/b1,b:a2/b2,a<b等价于a1*b2-a2*b1
  def cmp(a: (Int, Int), b: (Int, Int)): Int = {
    a._1 * b._2 - a._2 * b._1
  }

  //求交点，y=k1*x+b1,y=k2*x+b2,交点x=(b2-a2)/(k1-k2),保存为元组模式
  def inter(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    (b._2 - a._2, a._1 - b._1)
  }

  def make(all: ArrayBuffer[(Int, Int)], p: ArrayBuffer[(Int, Int)], line: ArrayBuffer[(Int, Int)]) {
    var n = all.size
    var temp = (0, 0)
    var i = 0
    var j = 0
    while (i < n) {
      j = i
      while (j < n && all(i)._1 == all(j)._1) {
        j += 1
      }
      i = j - 1

      if (line.isEmpty) {
        line.+=(all(i))
      } else {
        j = line.size - 1
        var flag = true
        while (j >= 0 && flag) {
          temp = inter(line(j), all(i))
          if (p.isEmpty) {
            flag = false
          } else {
            if (cmp(temp, p(p.size - 1)) <= 0) {
              p.remove(p.size - 1)
              line.remove(line.size - 1)
            } else
              flag = false
          }
          j -= 1
        }
        p.+=(temp)
        line.+=(all(i))
      }
      i += 1
    }
  }
}