//Two positive integers N and M are given. Integer N represents the number of chocolates arranged in a circle, numbered from 0 to N − 1.
//You start to eat the chocolates. After eating a chocolate you leave only a wrapper.
//You begin with eating chocolate number 0. Then you omit the next M − 1 chocolates or wrappers on the circle, and eat the following one.
//More precisely, if you ate chocolate number X, then you will next eat the chocolate with number (X + M) modulo N (remainder of division).
//You stop eating when you encounter an empty wrapper.
//For example, given integers N = 10 and M = 4. You will eat the following chocolates: 0, 4, 8, 2, 6.
//The goal is to count the number of chocolates that you will eat, following the above rules.
//Write a function:
//object Solution { def solution(N: Int, M: Int): Int }
//that, given two positive integers N and M, returns the number of chocolates that you will eat.
//For example, given integers N = 10 and M = 4. the function should return 5, as explained above.
//Assume that:
//N and M are integers within the range [1..1,000,000,000].
//Complexity:
//expected worst-case time complexity is O(log(N+M));
//expected worst-case space complexity is O(1).

//When we met with an empty wrapper, we must have been this position for twice. 
//We use i for the first time and j for the second time. Due to the modulo feature, 
//there must be nature number, to say k, so that: i * M + k * N = j * M. 
//Then we could easily prove that the smallest (earliest) i must be zero (for all i != 0, 
//    then (i-i) * M + k * N = (j-i) * M ). 
//    So the first eaten position would be first position that you meet again. 
//    Finally, the j would be the number of chocolates that you will eat.

package euclidean_algorithm

object ChocolatesByNumbers {
  def solution(N: Int, M: Int): Int = {
    //获取最小公倍数,可能会溢出
    var lcm = N.toLong * M / gcd(N, M)
    (lcm / M).toInt
  }

  //获取最大公约数
  def gcd(a: Int, b: Int): Int = {
    if (a % b == 0)
      b
    else
      gcd(b, a % b)
  }
}