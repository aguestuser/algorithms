package algos.sort

import algos.sort.Sort._
import algos.points._

import org.specs2.mutable.Specification

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */

class Sort$Test extends Specification {

  "merge sort" should {

    "sort an even-numbered list of distinct ints" in {
      mSort(List(8,4,2,7,6,1,3,5)) === List(1,2,3,4,5,6,7,8)
    }

    "sort an odd-numbered list of distinct ints" in {
      mSort(List(8,4,2,7,9,6,1,3,5)) === List(1,2,3,4,5,6,7,8,9)
    }

    "sort an list of ints with duplicates" in {
      mSort(List(4,4,2,2,6,1,3,5)) === List(1,2,2,3,4,4,5,6)
    }

    "sort a list of 100,000 ints" in {

      lazy val ints = io.Source.fromFile("src/test/resources/LotsOfInts.txt").getLines().map(_.toInt).toList
      lazy val expected = io.Source.fromFile("src/test/resources/LotsOfIntsSorted.txt").getLines().map(_.toInt).toList

      mSort(ints) === expected
    }

    "sort a list of Points by x coordinate" in {

      lazy val xUnsorted = List(Point(3,4),Point(2,5),Point(1,6))

      mSort(xUnsorted)(XOrdering) === List(Point(1,6),Point(2,5),Point(3,4))

    }

    "short a list of Points by Y coordinate" in {

      lazy val yUnsorted = List(Point(1,6),Point(2,5),Point(3,4))
      mSort(yUnsorted)(YOrdering) === List(Point(3,4),Point(2,5),Point(1,6))
    }
  }
}
