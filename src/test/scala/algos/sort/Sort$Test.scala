package algos.sort

import algos.points._
import algos.sort.Sort._
import org.specs2.mutable.Specification

import scala.collection.mutable.ArrayBuffer

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */

class Sort$Test extends Specification {

  lazy val wcl = (1000000 to 1 by -1).toList
  lazy val sl = (1 to 1000000).toList
  lazy val wcv = wcl.toVector
  lazy val sv = sl.toVector
  lazy val wcab = ArrayBuffer(wcl: _*)
  lazy val sab = ArrayBuffer(sl: _*)

  //TODO add test for sorting list of 1,000,000 random ints

  "merge sort on a list" should {

    "sort an even-numbered list of distinct ints" in {
      mSort(List(8,4,2,7,6,1,3,5)) === List(1,2,3,4,5,6,7,8)
    }

    "sort an odd-numbered list of distinct ints" in {
      mSort(List(8,4,2,7,9,6,1,3,5)) === List(1,2,3,4,5,6,7,8,9)
    }

    "sort an list of ints with duplicates" in {
      mSort(List(4,4,2,2,6,1,3,5)) === List(1,2,2,3,4,4,5,6)
    }

    "sort a list of 1,000,000 ints" in {
      mSort(wcl) === sl
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

  "merge sort on a vector" should {

    "sort an even-numbered vector of distinct ints" in {
      mSortV(Vector(8,4,2,7,6,1,3,5)) === Vector(1,2,3,4,5,6,7,8)
    }

    "sort an odd-numbered vector of distinct ints" in {
      mSortV(Vector(8,4,2,7,9,6,1,3,5)) === Vector(1,2,3,4,5,6,7,8,9)
    }

    "sort an vector of ints with duplicates" in {
      mSortV(Vector(4,4,2,2,6,1,3,5)) === Vector(1,2,2,3,4,4,5,6)
    }

    "sort a vector of 1,000,000 ints" in {
      mSortV(wcv) === sv
    }
  }

  "imperative quick sort on an array buffer" should {

    "sort an even-numbered list of distinct ints" in {
      qSort(ArrayBuffer(8, 4, 2, 7, 6, 1, 3, 5)) === ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8)
    }

    "sort an odd-numbered list of distinct ints" in {
      qSort(ArrayBuffer(8, 4, 2, 7, 9, 6, 1, 3, 5)) === ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "sort an list of ints with duplicates" in {
      qSort(ArrayBuffer(4, 4, 2, 2, 6, 1, 3, 5)) === ArrayBuffer(1, 2, 2, 3, 4, 4, 5, 6)
    }

    "sort a list of 1,000,000 ints" in {
      qSort(wcab) === sab
    }
  }

  "functional quick sort on a list" should {

    "sort an even-numbered list of distinct ints" in {
      qSortL(List(8,4,2,7,6,1,3,5)) === List(1,2,3,4,5,6,7,8)
    }

    "sort an odd-numbered list of distinct ints" in {
      qSortL(List(8,4,2,7,9,6,1,3,5)) === List(1,2,3,4,5,6,7,8,9)
    }

    "sort an list of ints with duplicates" in {
      qSortL(List(4,4,2,2,6,1,3,5)) === List(1,2,2,3,4,4,5,6)
    }

    "sort a list of 1,000,000 ints" in {
      qSortL(wcl) === sl
    }
  }
}
