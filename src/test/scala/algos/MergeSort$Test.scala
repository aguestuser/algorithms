package algos

import org.specs2.mutable.Specification

/**
 * Created by aguestuser on 1/24/15.
 */
class MergeSort$Test extends Specification {

  "MergeSort#sort" should {

    "sort an even-numbered array of distinct ints" in {
      MergeSort.sort(Array(8,4,2,7,6,1,3,5)) === Array(1,2,3,4,5,6,7,8)
    }

    "sort an odd-numbered array of distinct ints" in {
      MergeSort.sort(Array(8,4,2,7,9,6,1,3,5)) === Array(1,2,3,4,5,6,7,8,9)
    }

    "sort an array of ints with duplicates" in {
      MergeSort.sort(Array(4,4,2,2,6,1,3,5)) === Array(1,2,2,3,4,4,5,6)
    }
  }
}
