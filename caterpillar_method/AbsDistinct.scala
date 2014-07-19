//A non-empty zero-indexed array A consisting of N numbers is given. The array is sorted in non-decreasing order. The absolute distinct count of this array is the number of distinct absolute values among the elements of the array.
//For example, consider array A such that:
//  A[0] = -5    
//  A[1] = -3    
//  A[2] = -1
//  A[3] =  0    
//  A[4] =  3    
//  A[5] =  6
//The absolute distinct count of this array is 5, because there are 5 distinct absolute values among the elements of this array, namely 0, 1, 3, 5 and 6.
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that, given a non-empty zero-indexed array A consisting of N numbers, returns absolute distinct count of array A.
//For example, given array A such that:
//  A[0] = -5    
//  A[1] = -3    
//  A[2] = -1
//  A[3] =  0    
//  A[4] =  3    
//  A[5] =  6
//the function should return 5, as explained above.
//Assume that:
//N is an integer within the range [1..100,000];
//each element of array A is an integer within the range [−2,147,483,648..2,147,483,647];
//array A is sorted in non-decreasing order.
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package caterpillar_method

import scala.math._

object AbsDistinct {
  def solution(A: Array[Int]): Int = {
    val N = A.length
    var result = 0
    var i = 0
    var j = N - 1
    var prevAbs = -1.toLong
    while (i <= j) {
      var absLeft = abs(A(i).toLong)
      var absRight = abs(A(j).toLong)
      var bigAbs = max(absLeft, absRight)
      if (absLeft > absRight)
        i += 1
      else
        j -= 1
      if (prevAbs != bigAbs)
        result += 1
      prevAbs = bigAbs
    }
    result
  }
}