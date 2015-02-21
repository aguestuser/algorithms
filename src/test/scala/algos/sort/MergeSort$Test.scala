package algos.sort

import algos.sort.MergeSort._
import algos.points._

import org.specs2.mutable.Specification

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */

class MergeSort$Test extends Specification {

  "MergeSort#sort" should {

    "sort an even-numbered vector of distinct ints" in {
      sort(Vector(8,4,2,7,6,1,3,5)) === Vector(1,2,3,4,5,6,7,8)
    }

    "sort an odd-numbered vector of distinct ints" in {
      sort(Vector(8,4,2,7,9,6,1,3,5)) === Vector(1,2,3,4,5,6,7,8,9)
    }

    "sort an vector of ints with duplicates" in {
      sort(Vector(4,4,2,2,6,1,3,5)) === Vector(1,2,2,3,4,4,5,6)
    }

    "sort a vector of 100,000 ints" in {

      lazy val ints = io.Source.fromFile("src/test/resources/LotsOfInts.txt").getLines().map(_.toInt).toVector
      lazy val expected = io.Source.fromFile("src/test/resources/LotsOfIntsSorted.txt").getLines().map(_.toInt).toVector

      sort(ints) === expected
    }

    "sort a vector of Points by x coordinate" in {

      lazy val xUnsorted = Vector(Point(3,4),Point(2,5),Point(1,6))

      sort(xUnsorted)(XOrdering) === Vector(Point(1,6),Point(2,5),Point(3,4))

    }

    "short a vector of Points by Y coordinate" in {

      lazy val yUnsorted = Vector(Point(1,6),Point(2,5),Point(3,4))
      sort(yUnsorted)(YOrdering) === Vector(Point(3,4),Point(2,5),Point(1,6))
    }
  }
}
