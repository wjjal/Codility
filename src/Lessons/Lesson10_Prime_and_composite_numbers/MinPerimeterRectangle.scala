//An integer N is given, representing the area of some rectangle.
//The area of a rectangle whose sides are of length A and B is A * B, and the perimeter is 2 * (A + B).
//The goal is to find the minimal perimeter of any rectangle whose area equals N. The sides of this rectangle should be only integers.
//For example, given integer N = 30, rectangles of area 30 are:
//(1, 30), with a perimeter of 62,
//(2, 15), with a perimeter of 34,
//(3, 10), with a perimeter of 26,
//(5, 6), with a perimeter of 22.
//Write a function:
//object Solution { def solution(N: Int): Int }
//that, given an integer N, returns the minimal perimeter of any rectangle whose area is exactly equal to N.
//For example, given an integer N = 30, the function should return 22, as explained above.
//Assume that:
//N is an integer within the range [1..1,000,000,000].
//Complexity:
//expected worst-case time complexity is O(sqrt(N));
//expected worst-case space complexity is O(1).

package Lessons.Lesson10_Prime_and_composite_numbers

import scala.math._

object MinPerimeterRectangle {
  def solution(N: Int): Int = {
    var result = N + 1
    for (i <- 2 to sqrt(N).toInt)
      if (N % i == 0)
        result = min(result, N / i + i)
    result << 1
  }
}