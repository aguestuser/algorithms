package algos.sort

import algos.points._
import algos.sort.Sort._

import org.specs2.mutable.Specification

import scala.math.random
import scala.collection.mutable.ArrayBuffer

/**
 * Author: @aguestuser
 * Date: 1/24/15
 * License: GPLv2
 */

class Sort$Test extends Specification {

  lazy val wcl = (1000000 to 1 by -1).toList
  lazy val sl = (1 to 1000000).toList
  lazy val rl = List.fill(1000000)((random * 1000000).toInt)

  lazy val wcv = wcl.toVector
  lazy val sv = sl.toVector
  lazy val rv = rl.toVector

  lazy val wcab = ArrayBuffer(wcl: _*)
  lazy val sab = ArrayBuffer(sl: _*)
  lazy val rab = ArrayBuffer(rl: _*)

  "didSort" should {

    "correctly identify the properties of a sorted output given an unsorted input" in {
      didSort(List(3,2,1),List(1,2,3)) === true
      didSort(List(3,2,1),List(2,3,4)) === false // same set, correct bounds
      didSort(List(3,2),List(1,2,3)) === false // same size
      didSort(List(1,2,3),List(2,1,3)) === false // ordering
    }
  }

//  "countCompares" should {
//
//    lazy val ints = ArrayBuffer(io.Source.fromFile("src/test/resources/QuickSortSampleInts.txt").getLines.map(_.toInt).toSeq: _*)
//    // always choose first: 162085
//    // always choose last: 164123
//    // choose median of first, middle, last: 138382
//    // choose randomly: 155544, 146886,
//    "get correct count" in {
//      countCompares[Int](ints) === 138382
//    }
//  }

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

    "sort a list of 1,000,000 reverse-sorted ints" in {
      mSort(wcl) === sl
    }

    "sort a list of 10,000 random ints" in {
      didSort(rl,mSort(rl)) === true
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
      mSort(Vector(8,4,2,7,6,1,3,5)) === Vector(1,2,3,4,5,6,7,8)
    }

    "sort an odd-numbered vector of distinct ints" in {
      mSort(Vector(8,4,2,7,9,6,1,3,5)) === Vector(1,2,3,4,5,6,7,8,9)
    }

    "sort an vector of ints with duplicates" in {
      mSort(Vector(4,4,2,2,6,1,3,5)) === Vector(1,2,2,3,4,4,5,6)
    }

    "sort a vector of 1,000,000 ints" in {
      mSort(wcv) === sv
    }

    "sort a vector of 10,000 random ints" in {
      didSort(rv,mSort(rv)) === true
    }
  }

  "imperative quick sort on an array buffer" should {

    "sort an even-numbered array of distinct ints" in {
      qSort(ArrayBuffer(8, 4, 2, 7, 6, 1, 3, 5)) === ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8)
    }

    "sort an odd-numbered array of distinct ints" in {
      qSort(ArrayBuffer(8, 4, 2, 7, 9, 6, 1, 3, 5)) === ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "sort an array of ints with duplicates" in {
      qSort(ArrayBuffer(4, 4, 2, 2, 6, 1, 3, 5)) === ArrayBuffer(1, 2, 2, 3, 4, 4, 5, 6)
    }

    "sort a array of 1,000,000 ints" in {
      qSort(wcab) === sab
    }

    "sort a array of 10,000 random ints" in {
      didSort(rab,qSort(rab)) === true
    }
  }

  "functional quick sort on a list" should {

    "sort an even-numbered list of distinct ints" in {
      qSort(List(8,4,2,7,6,1,3,5)) === List(1,2,3,4,5,6,7,8)
    }

    "sort an odd-numbered list of distinct ints" in {
      qSort(List(8,4,2,7,9,6,1,3,5)) === List(1,2,3,4,5,6,7,8,9)
    }

    "sort an list of ints with duplicates" in {
      qSort(List(4,4,2,2,6,1,3,5)) === List(1,2,2,3,4,4,5,6)
    }

    "sort a list of 1,000,000 ints" in {
      qSort(wcl) === sl
    }

    "sort a list of 10,000 random ints" in {
      didSort(rl,qSort(rl)) === true
    }
  }

  "functional random select" should {

    "select the kth biggest element in a list of distinct ints" in {
      rSelect(List(6,1,2,5,4,3), 2) === 2
      rSelect(List(6,1,2,5,4,3), 3) === 3
      rSelect(List(6,1,2,5,4,3), 6) === 6
      rSelect(wcl,500) === 500
    }

    "select the kth biggest element in a list of ints w/ dups" in {
      rSelect(List(4,5,3,2,1,2),2) === 2
      rSelect(List(4,5,3,2,1,2),3) === 2
    }
  }

  "imperative random select" should {

    "select the kth biggest element in an array of distinct ints" in {
      rSelect(ArrayBuffer(6,1,2,5,4,3), 2) === 2
      rSelect(ArrayBuffer(6,1,2,5,4,3), 3) === 3
      rSelect(ArrayBuffer(6,1,2,5,4,3), 6) === 6
      rSelect(ArrayBuffer(10,5,0),2) === 5
      rSelect(ArrayBuffer(10,5,0),1) === 0
      rSelect(ArrayBuffer(10,5,0),3) === 10
    }
    "select the kth biggest element in an array of ints w/ dups" in {
      rSelect(ArrayBuffer(4,5,3,2,1,2),2) === 2
      rSelect(ArrayBuffer(4,5,3,2,1,2),3) === 2
    }
  }
}
