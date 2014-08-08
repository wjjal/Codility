//N countries (numbered from 0 to N−1) participate in a space mission. Each country has trained a certain number of astronauts and each country has to delegate a certain number of astronauts to the mission's crew. How many different ways are there to select the crew?
//For example, suppose there are three countries A-land, B-land and C-land and
//A-land has 6 astronauts;
//B-land has 4 astronauts;
//C-land has 7 astronauts.
//and
//A-land has to delegate 1 astronaut;
//B-land has to delegate 3 astronauts;
//C-land has to delegate 4 astronauts.
//Then
//there are 6 different ways in which A-land can delegate 1 out of 6 astronauts;
//there are 4 different ways in which B-land can delegate 3 out of 4 astronauts;
//there are 35 different ways in which C-land can delegate 4 out of 7 astronauts.
//Each country's choice is independent, so the total number of different ways to build the mission crew is 6*4*35=840.
//Write a function
//object Solution { def solution(T: Array[Int], D: Array[Int]): Int }
//that, given two non-empty zero-indexed arrays T and D consisting of N integers each, returns the number of different ways in which the space crew can be selected, where:
//T[K] = number of astronauts in country K;
//D[K] = number of astronauts to be delegated from country K.
//Assume that:
//N is an integer within the range [1..1,000];
//each element of arrays T, D is an integer within the range [0..1,000,000];
//T[i] ≥ D[i] for i=0..(N−1).
//For example, given N=3 and
//T[0] = 6  T[1] = 4  T[2] = 7
//D[0] = 1  D[1] = 3  D[2] = 4
//the function should return 840, as explained above. If the result exceeds 1,410,000,016, the function should return the remainder of the result modulo 1,410,000,017.
//Complexity:
//expected worst-case time complexity is O(max(T)*log(max(T))+N);
//expected worst-case space complexity is O(N+max(T)), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package challenges

object Kappa2011 {
  val M = 1410000017
  def solution(T: Array[Int], D: Array[Int]): Int = {
    var N = T.length
    var m = 0
    for (i <- 0 until N) {
      if (m < T(i))
        m = T(i)
    }
    //f(i)=i!
    var f = new Array[Int](M + 1)
    var rf = new Array[Int](M + 1)
    f(0) = 1
    rf(0) = 1
    for (i <- 1 to m) {
      f(i) = mul(f(i - 1), i)
    }
    var ans = 1
    for (i <- 0 until N) {
      if (rf(D(i)) == 0)
        rf(D(i)) = rev(f(D(i)))
      if (rf(T(i) - D(i)) == 0)
        rf(T(i) - D(i)) = rev(f(T(i) - D(i)))
      ans = mul(mul(ans, f(T(i))), mul(rf(D(i)), rf(T(i) - D(i))))
    }
    ans
  }

  def mul(x: Long, y: Long): Int = {
    (x * y % M).toInt
  }

  def rev(X: Int): Int = {
    var x = X
    var re = 1
    var i = M - 2
    while (i > 0) {
      if ((i & 1) != 0)
        re = mul(re, x)
      x = mul(x, x)
      i >>= 1
    }
    re
  }
}