//Do you like puzzles? Here is one for you. You are given a positive integer A. The goal is to construct the shortest possible sequence of integers ending with A, using the following rules:
//the first element of the sequence is 1,
//each of the successive elements is the sum of any two preceding elements (adding a single element to itself is also permissible),
//each element is larger than all the preceding elements; that is, the sequence is increasing.
//For example, for A = 42, a possible solution is [1, 2, 3, 6, 12, 24, 30, 42]. Another possible solution is [1, 2, 4, 5, 8, 16, 21, 42].
//Write a function:
//object Solution { def solution(A: Int): Array[Int] }
//that, given an integer A, returns the shortest possible sequence of integers satisfying the above conditions and ending with A.
//The sequence should be returned as:
//a structure Results (in C), or
//a vector of integers (in C++), or
//a record Results (in Pascal), or
//an array of integers (in any other programming language).
//For example, given A = 42, the function may return the sequence [1, 2, 3, 6, 12, 24, 30, 42], as explained above.
//Assume that:
//A is an integer within the range [1..600].

package Challenges.challenges_2012

import scala.collection.mutable.Stack

object Rho2012 {
  def main(args: Array[String]) {
    val A = 42
    for(elem<-solution(A)){
      print(elem+ " ")
    }
  }

  def solution(A: Int): Array[Int] = {
    var stack = new Stack[Int]()
    stack.push(1)
    var len = 1
    while (!dfs(A, stack, len)) {
      len += 1
    }
    stack.reverse.toArray
  }

  def dfs(A: Int, stack: Stack[Int], len: Int): Boolean = {
    if (stack.top == A)
      return true
    if (stack.size >= len)
      return false
    var i = stack.top
    var j = len - stack.size
    //最大数翻倍
    while (i < A && j > 0) {
      i <<= 1
      j -= 1
    }
    if (i < A)
      return false
    i = 0
    while (i < stack.size && (stack(i) << 1) > stack.top) {
      var j = i
      var x = stack(i) + stack(j)
      while (j < stack.size && x > stack.top) {
        if (x <= A) {
          stack.push(x)
          if (dfs(A, stack, len))
            return true
          stack.pop
        }
        j += 1
        if (j < stack.size)
          x = stack(i) + stack(j)
      }
      i += 1
    }
    return false
  }
}