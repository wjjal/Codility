//A zero-indexed array A consisting of N integers is given. An inversion is a pair of indexes (P, Q) such that P < Q and A[Q] < A[P].
//Write a function:
//object Solution { def solution(A: Array[Int]): Int }
//that computes the number of inversions in A, or returns −1 if it exceeds 1,000,000,000.
//Assume that:
//N is an integer within the range [0..100,000];
//each element of array A is an integer within the range [−2,147,483,648..2,147,483,647].
//For example, in the following array:
//A[0] = -1 A[1] = 6 A[2] = 3
//A[3] =  4 A[4] = 7 A[5] = 4
//there are four inversions:
//  (1,2)  (1,3)  (1,5)  (4,5)
//so the function should return 4.
//Complexity:
//expected worst-case time complexity is O(N*log(N));
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package future_training

object ArrayInversionCount {
  def main(args: Array[String]) {
     val A = Array(-1,6,3,4,7,4)
     solution(A)
  }
  
  def solution(A: Array[Int]): Int = {
    mergeSort(A, 0, A.length - 1)
  }

  def mergeSort(A: Array[Int], start: Int, end: Int): Int = {
    var mid = (start + end) >> 1
    if (start >= end)
      return 0
    var left_inver = mergeSort(A, start, mid)
    var right_inver = mergeSort(A, mid + 1, end)
    println(left_inver+" "+right_inver)
    for(i<- start to end)
      print(A(i))
    println()
    var i = start
    var j = mid + 1
    var temp = new Array[Int](end - start + 1)
    var k = 0
    var merge_inver = 0
    while (i <= mid || j <= end) {
      if (i > mid) {
        temp(k) = A(j)
        j += 1
        k += 1
      } else if (j > end) {
        merge_inver += j - mid - 1
        temp(k) = A(i)
        i += 1
        k += 1
      } else {
        if (A(i) <= A(j)) {
          merge_inver += j - mid - 1
          temp(k) = A(i)
          i += 1
          k += 1
        } else {
          temp(k) = A(j)
          j += 1
          k += 1
        }
      }
    }

    for (k <- 0 until temp.length)
      A(start + k) = temp(k)
    return merge_inver + left_inver + right_inver
  }
}