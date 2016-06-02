//There are N+1 intersections in a prison, connected by N corridors, and one can move between any two intersections using the corridors. Intersections with only one corridor are located near the exits from the prison.
//There is a cell block near every intersection. Exactly M cell blocks hold prisoners; others are empty. Due to a malfunction in the locking system, all the cells have been opened. The situation is critical and the governor of the prison needs to know where to position the guards in order to prevent the prisoners from escaping.
//You are given a map of the prison in the form of two zero-indexed arrays A, B of length N and a zero-indexed array C of length M:
//For each I (0 ≤ I < N) there is a corridor between intersections A[I] and B[I].
//For each I (0 ≤ I < M) there are prisoners in the cell block near intersection C[I].
//A guard can be located at an intersection, but not at an intersections near to a cell block which initially held prisoners. Prisoners from a cell block located near intersection X can escape from the prison if there is a path from X to an intersection located near an exit from the prison and there are no guards at any intersection along this path. The governor would like to know the minimum number of guards he must deploy in the operation.
//Write a function:
//object Solution { def solution(A: Array[Int], B: Array[Int], C: Array[Int]): Int }
//that, given arrays A, B of N integers and array C of M integers, returns the minimum number of guards that can prevent all prisoners from escaping. If there is no way to prevent an escape of some prisoner, the function should return −1.
//For example, given the following arrays:
//    A[0] = 0    B[0] = 1    C[0] = 1
//    A[1] = 1    B[1] = 2    C[1] = 6
//    A[2] = 2    B[2] = 3
//    A[3] = 3    B[3] = 4
//    A[4] = 3    B[4] = 5
//    A[5] = 2    B[5] = 6
//    A[6] = 6    B[6] = 8
//    A[7] = 6    B[7] = 7
//
//the function should return 4. Four guards can be positioned at the intersections numbered 0, 3, 7 and 8 (another solution would be to position guards at intersections 0, 2, 7 and 8). By positioning three or fewer guards, the governor is unable to prevent some prisoners from escaping.
//Assume that:
//N is an integer within the range [1..200,000];
//M is an integer within the range [0..N+1];
//each element of arrays A, B, C is an integer within the range [0..N];
//the elements of C are all distinct;
//distance from intersection 0 to any other intersection is not greater than 500.
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Challenges.challenges_2014

import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer

object Phosphorus2014 {
  def main(args: Array[String]) {
    val A = Array(0, 1, 2, 3, 3, 2, 6, 6)
    val B = Array(1, 2, 3, 4, 5, 6, 8, 7)
    val C = Array(1, 6)
    println(solution(A, B, C))
  }
  
//0 逃犯无到根的路 从根到叶子无通路
//1 逃犯无到根的路 从根到叶子有通路
//2 逃犯有到根的路 从根到叶子无通路
//
//0 >= 2
//0 >= 1

  //x取较小值，-1表示无穷大
  def make(x: Int, y: Int): Int = {
    if (y >= 0 && (x < 0 || x > y)) y else x
  }

  def give(cur: Int, edge: Array[ArrayBuffer[Int]], have: Array[Array[Int]], parent: Int, state1: Int, state2: Int): Int = {
    var sum = 0
    for (i <- 0 until edge(cur).length) {
      if (parent != edge(cur)(i)) {
        var temp = have(state1)(edge(cur)(i))
        if (state2 > 0)
          temp = make(temp, have(state2)(edge(cur)(i)))
        if (temp < 0)
          return -1
        sum += temp
      }
    }
    sum
  }

  def dfs(mark: Array[Boolean], edge: Array[ArrayBuffer[Int]], have: Array[Array[Int]], cur: Int, parent: Int) {
    if (edge(cur).length == 1) {
      have(0)(cur) = 1
      have(1)(cur) = 0
      have(2)(cur) = 1
      return
    }
    for (i <- 0 until edge(cur).length) {
      if (parent != edge(cur)(i))
        dfs(mark, edge, have, edge(cur)(i), cur)
    }
    have(2)(cur) = give(cur, edge, have, parent, 2, -1)
    if (mark(cur)) {
      have(0)(cur) = -1
      have(1)(cur) = -1
      return
    }
    have(0)(cur) = give(cur, edge, have, parent, 0, -1)
    var may = give(cur, edge, have, parent, 1, 2)
    if (may >= 0)
      have(0)(cur) = make(have(0)(cur), may + 1)
    have(1)(cur) = give(cur, edge, have, parent, 1, -1)
    have(1)(cur) = make(have(1)(cur), have(0)(cur))
    have(2)(cur) = make(have(2)(cur), have(0)(cur))
  }

  def solution(A: Array[Int], B: Array[Int], C: Array[Int]): Int = {
    val N = A.length
    val M = C.length
    if (M == 0)
      return 0
    var edge = new Array[ArrayBuffer[Int]](N + 1)
    var c_pos = new Array[Boolean](N + 1)
    for (i <- 0 until N + 1)
      edge(i) = new ArrayBuffer[Int]()
    for (i <- 0 until N) {
      edge(A(i)).+=(B(i))
      edge(B(i)).+=(A(i))
    }
    for (i <- 0 until M) {
      c_pos(C(i)) = true
      if (edge(C(i)).size == 1)
        return -1
    }
    var have = new Array[Array[Int]](3)
    for (i <- 0 until 3)
      have(i) = new Array[Int](N + 1)
    dfs(c_pos, edge, have, C(0), -1)
    return have(2)(C(0))
  }

  //未通过测试
  def mysolution(A: Array[Int], B: Array[Int], C: Array[Int]): Int = {
    val N = A.length
    val M = C.length
    if (M == 0)
      return 0
    var edge = new Array[ArrayBuffer[Int]](N + 1)
    var c_pos = new Array[Boolean](N + 1)
    var stack = new Stack[Int]()
    var visit = new Array[Boolean](N + 1)
    var router = new Array[Int](N + 1)
    for (i <- 0 until N + 1)
      edge(i) = new ArrayBuffer[Int]()
    for (i <- 0 until N) {
      edge(A(i)).+=(B(i))
      edge(B(i)).+=(A(i))
    }
    for (i <- 0 until M) {
      c_pos(C(i)) = true
      if (edge(C(i)).size == 1)
        return -1
    }

    for (i <- 0 until N + 1) {
      if (edge(i).size == 1) {
        if (c_pos(i))
          return -1
        stack.push(i)
        visit(i) = true
        router(i) = i
      }
    }
    var close = new ArrayBuffer[Int]()
    while (!stack.isEmpty) {
      var cur = stack.pop
      //need若为true,表示cur有子节点或递归子节点为犯人小黑屋,此时，cur节点必须放置守卫
      var need = false
      var ta = new ArrayBuffer[Int]()
      for (node <- edge(cur)) {
        var parent = cur
        var child = node
        if (!visit(child) && !c_pos(child)) {
          while (edge(child).size == 2 && !c_pos(child)) {
            var find = false
            for (next <- edge(child)) {
              if (next != parent && !find) {
                visit(child) = true
                parent = child
                child = next
                find = true
              }
            }
          }
        }
        if (!c_pos(child) && !visit(child))
          ta.+=(child)
        else if (c_pos(child))
          need = true
        if (!c_pos(child) && visit(child))
          router(child) = child
      }
      if (need)
        close.+=(cur)
      else {
        for (e <- ta) {
          stack.push(e)
          router(e) = cur
          visit(e) = true
        }
      }
    }
    var ans = new HashSet[Int]()
    for (node <- close) {
      var cur = node
      while (cur != router(cur))
        cur = router(cur)
      ans.+=(cur)
    }
    ans.size
  }
}