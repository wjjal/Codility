//A company of dwarfs is travelling across the New Zealand. On reaching the Clutha River the dwarfs need to get across, but recent storms have washed away the bridge. Luckily, a small ferry, in the form of a square raft, is operating.
//
//The raft is square and has N rows of seats, numbered from 1 to N. Each row contains N seats, labeled with consecutive letters (A, B, C, etc.). Each seat is identified by a string composed of its row number followed by its column number; for example, "9C" denotes the third seat in the 9th row.
//
//  The raft has already been loaded with barrels in some seat positions, and other seats are already occupied by dwarfs. Our company of dwarfs may only take the remaining unoccupied seats. The ferryman wants to accommodate as many dwarfs as possible, but the raft needs to be stable when making the crossing. That is, the following conditions must be satisfied:
//
//  the front and back halves of the raft (in terms of the rows of seats) must each contain the same number of dwarfs;
//similarly, the left and right sides of the raft (in terms of the columns of seats) must each contain the same number of dwarfs.
//  You do not have to worry about balancing the barrels; you can assume that their weights are negligible.
//
//For example, a raft of size N = 4 is shown in the following illustration:
//
//
//
//  Barrels are marked as brown squares, and seats that are already occupied by dwarfs are labeled d.
//
//  The positions of the barrels are given in string S. The occupied seat numbers are given in string T. The contents of the strings are separated by single spaces and may appear in any order. For example, in the diagram above, S = "1B 1C 4B 1D 2A" and T = "3B 2D".
//
//  In this example, the ferryman can accommodate at most six more dwarfs, as indicated by the green squares in the following diagram:
//
//
//
//  The raft is then balanced: both left and right halves have the same number of dwarfs (four), and both front and back halves have the same number of dwarfs (also four).
//
//  Write a function:
//
//object Solution { def solution(N: Int, S: String, T: String): Int }
//
//that, given the size of the raft N and two strings S, T that describes the positions of barrels and occupied seats, respectively, returns the maximum number of dwarfs that can fit on the raft. If it is not possible to balance the raft with dwarfs, your function should return -1.
//
//For instance, given N = 4, S = "1B 1C 4B 1D 2A" and T = "3B 2D", your function should return 6, as explained above.
//
//  Assume that:
//
//  N is an even integer within the range [2..26];
//strings S, T consist of valid seat numbers, separated with spaces;
//each seat number can appear no more than once in the strings;
//no seat number can appear in both S and T simultaneously.
//  In your solution, focus on correctness. The performance of your solution will not be the focus of the assessment.

package Lessons.Lesson90_TasksFromIndeedPrime2016Challenge


object DwarfsRafting {
  def main(args: Array[String]) {
    val N = 4
    val S = "1B 1C 4B 1D 2A"
    val T = "3B 2D"
    println(solution(N, S, T))
  }

  def solution(N: Int, S: String, T: String): Int = {
    val a = N / 2
    val b = if (N % 2 == 0) N / 2 + 1 else N / 2 + 2
    var left_high = a * a
    var left_low = a * a
    var right_high = a * a
    var right_low = a * a
    val s = S.split(" ")
    val t = T.split(" ")
    s.filter(_.length > 0).foreach(x => {
      val i = x.substring(0, x.length - 1).toInt
      val j = x.charAt(x.length - 1) - 'A' + 1
      if (i <= a && j <= a) {
        left_high -= 1
      } else if (i <= a && j >= b) {
        left_low -= 1
      } else if (i >= b && j <= a) {
        right_high -= 1
      } else if (i >= b && j >= b) {
        right_low -= 1
      }
    })

    val max_1 = math.min(left_high, right_low)
    val max_2 = math.min(right_high, left_low)
    var left_high_t = 0
    var left_low_t = 0
    var right_high_t = 0
    var right_low_t = 0

    t.filter(_.length > 0).foreach(x => {
      val i = x.substring(0, x.length - 1).toInt
      val j = x.charAt(x.length - 1) - 'A' + 1
      if (i <= a && j <= a) {
        left_high_t += 1
      } else if (i <= a && j >= b) {
        left_low_t += 1
      } else if (i >= b && j <= a) {
        right_high_t += 1
      } else if (i >= b && j >= b) {
        right_low_t += 1
      }
    })
    if (math.max(left_high_t, right_low_t) > max_1 || math.max(right_high_t, left_low_t) > max_2) {
      return -1
    }
    max_1 * 2 + max_2 * 2 - left_high_t - left_low_t - right_high_t - right_low_t
  }
}
