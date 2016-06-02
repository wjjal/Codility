//There is an old dry well. Its sides are made of concrete rings. Each such ring is one meter high, but the rings can have different (internal) diameters. Nevertheless, all the rings are centered on one another. The well is N meters deep; that is, there are N concrete rings inside it.
//You are about to drop M concrete disks into the well. Each disk is one meter thick, and different disks can have different diameters. Once each disk is dropped, it falls down until:
//it hits the bottom of the well;
//it hits a ring whose internal diameter is smaller then the disk's diameter; or
//it hits a previously dropped disk.
//(Note that if the internal diameter of a ring and the diameter of a disk are equal, then the disk can fall through the ring.)
//The disks you are about to drop are ready and you know their diameters, as well as the diameters of all the rings in the well. The question arises: how many of the disks will fit into the well?
//Write a function:
//object Solution { def solution(A: Array[Int], B: Array[Int]): Int }
//that, given two zero-indexed arrays of integers − A, containing the internal diameters of the N rings (in top-down order), and B, containing the diameters of the M disks (in the order they are to be dropped) − returns the number of disks that will fit into the well.
//For example, given the following two arrays:
//  A[0] = 5    B[0] = 2
//  A[1] = 6    B[1] = 3
//  A[2] = 4    B[2] = 5
//  A[3] = 3    B[3] = 2
//  A[4] = 6    B[4] = 4
//  A[5] = 2
//  A[6] = 3
//the function should return 4, as all but the last of the disks will fit into the well. The figure shows the situation after dropping four disks.
//
//Assume that:
//N and M are integers within the range [1..200,000];
//each element of arrays A, B is an integer within the range [1..1,000,000,000].
//Complexity:
//expected worst-case time complexity is O(N);
//expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
//Elements of input arrays can be modified.

package Challenges.challenges_2013

import scala.math._

object Omega2013 {
  def solution(A: Array[Int], B: Array[Int]): Int = {
    val N = A.length
    for (i <- 1 until N) {
      A(i) = min(A(i - 1), A(i))
    }
    var num = 0
    var cur = N - 1
    for (disk <- B) {
      while (cur >= 0 && A(cur) < disk)
        cur -= 1
      if (cur >= 0) {
        cur -= 1
        num += 1
      } else
        return num
    }
    num
  }
}