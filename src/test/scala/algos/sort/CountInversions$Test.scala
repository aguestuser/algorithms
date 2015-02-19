package algos.sort

import algos.sort.CountInversions._

import org.specs2.mutable.Specification

/**
 * Author: @aguestuser
 * Date: 2/18/15
 * License: GPLv2
 */

class CountInversions$Test extends Specification {

  "Inversions module" should {

    "find the number of inversions in an array of ints" in {

      countInversions(Array(1,2,3)) === 0
      countInversions(Array(1,3,5,2,4,6)) === 3
      countInversions(Array(3,2,1,6,5,4)) === 6
      countInversions(Array(3,2,6,1,5,4)) === 7


    }
  }

}
