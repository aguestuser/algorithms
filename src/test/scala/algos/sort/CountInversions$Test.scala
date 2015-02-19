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

    "count inversions in a vector of ints" in {

      countInversions(Vector(1,2,3)) === 0
      countInversions(Vector(1,3,5,2,4,6)) === 3
      countInversions(Vector(3,2,1,6,5,4)) === 6
      countInversions(Vector(3,2,6,1,5,4)) === 7
    }

    "count inversions in a vector of 100,000 ints" in {

      lazy val ints: Vector[Int] = io.Source.fromFile("src/test/resources/LotsOfInts.txt").getLines().map(_.toInt).toVector
      countInversions(ints) === 2407905288L

    }
  }

}
