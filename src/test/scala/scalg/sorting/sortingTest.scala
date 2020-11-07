package scalg.sorting

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class sortingTest extends AnyFunSuite {
  test("heapSort() does nothing to an empty array") {
    val a = Array[Int]()
    assert(a.heapSort sameElements Array[Int]())
  }

  test("heapSort() shouldn't change array that already sorted") {
    val a = Array(1, 2, 3, 4, 5, 6, 7)
    assert(a.heapSort sameElements Array(1, 2, 3, 4, 5, 6, 7))

    val b = Array(3)
    assert(b.heapSort sameElements Array(3))
  }

  test("heapSort() should resort backward sorted array") {
    val a = Array(7, 6, 5, 4, 3, 2, 1)
    assert(a.heapSort sameElements Array(1, 2, 3, 4, 5, 6, 7))
  }

  test("heapSort() should random array") {
    val a = Array(3, 1, 2, 5, 7, 8, 6)

    assert(a.heapSort sameElements Array(1, 2, 3, 5, 6, 7, 8))
  }
}
