//Recently, more and more illegal street races have been spotted at night in the city, and they have become a serious threat to public safety. Therefore, the Police Chief has decided to deploy speed cameras on the streets to collect evidence.
//There are N+1 intersections in the city, connected by N roads. Every road has the same length of 1. A street race may take place between any two different intersections by using the roads connecting them. Limited by their budget, the police are able to deploy at most K speed cameras on these N roads. These K speed cameras should be installed such that the length of any possible street race route not covered by speed cameras should be as short as possible.
//You are given a map of the city in the form of two zero-indexed arrays, A and B of length N, and an integer K:
//For each J (0 ≤ J < N) there is a road connecting intersections A[J] and B[J].
//The Police Chief would like to know the minimum length of the longest path out of surveillance after placing at most K speed cameras.
//
//Write a function:
//object Solution { def solution(A: Array[Int], B: Array[Int], K: Int): Int }
//that, given arrays A and B of N integers and integer K, returns the minimum length of the longest path unmonitored by speed cameras after placing at most K speed cameras.
//For example, given K = 2 and the following arrays:
//A[0] = 5    B[0] = 1
//A[1] = 1    B[1] = 0
//A[2] = 0    B[2] = 7
//A[3] = 2    B[3] = 4
//A[4] = 7    B[4] = 2
//A[5] = 0    B[5] = 6
//A[6] = 6    B[6] = 8
//A[7] = 6    B[7] = 3
//A[8] = 1    B[8] = 9
//
//
//the function should return 2. Two speed cameras can be installed on the roads between intersections 1 and 0 and between intersections 0 and 7. (Another solution would be to install speed cameras between intersections 0 and 7 and between intersections 0 and 6.) By installing speed cameras according the first plan, one of the longest paths without a speed camera starts at intersection 8, passes through intersection 6 and ends at intersection 3, which consists of two roads. (Other longest paths are composed of intersections 5, 1, 9 and 7, 2, 4).
//Assume that:
//N is an integer within the range [1..50,000];
//each element of arrays A, B is an integer within the range [0..N];
//K is an integer within the range [0..N];
//the distance between any two intersections is not greater than 900.
//Complexity:
//
//expected worst-case time complexity is O(N*log(N));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Challenges.Challenges_2015

import scala.collection.mutable.ArrayBuffer
import scala.math._

object Calcium2015 {
  def main(args: Array[String]) {
    val A = Array(0, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 10, 11, 11)
    val B = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    val K = 4
    println(solution(A, B, K))
  }

  class Node(val idx: Int, var parent: Node = null) {
    var neighbours = new ArrayBuffer[Node]
    var children = new ArrayBuffer[Node]
    var height = -1
  }

  def solution(A: Array[Int], B: Array[Int], K: Int): Int = {
    val N = A.length + 1
    var graph = new Array[Node](N)
    for (i <- 0 until N) {
      graph(i) = new Node(i)
    }
    for ((v1, v2) <- A zip B) {
      graph(v1).neighbours.append(graph(v2))
      graph(v2).neighbours.append(graph(v1))
    }
    root(graph(0))
    var left = 0
    var right = 900
    while (left < right) {
      var mid = (left + right) >> 1
      var need = needed_carmeras(graph(0), mid)
      if (need <= K) right = mid else left = mid + 1
    }
    left
  }

  def root(node: Node, parent: Node = null): Unit = {
    node.parent = parent
    for (child <- node.neighbours) {
      if (child != parent) {
        node.children.append(child)
        root(child, node)
      }
    }
  }

  def needed_carmeras(node: Node, allowed_dis: Int): Int = {
    var re = 0
    node.height = 0
    for (child <- node.children) {
      re += needed_carmeras(child, allowed_dis)
    }
    var sigh = node.children.filter(_.height < allowed_dis).sortBy(x => (x.height))
    re += node.children.length - sigh.length
    var p = sigh.length - 1
    while (p > 0 && sigh(p).height + sigh(p - 1).height + 2 > allowed_dis) {
      re += 1
      p -= 1
    }
    for (i <- 0 to p) {
      node.height = max(node.height, sigh(i).height + 1)
    }
    re
  }
}