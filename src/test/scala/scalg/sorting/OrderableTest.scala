package scalg.sorting

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import scalg.sorting.array2SortableArray
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OrderableTest extends AnyFunSuite {
  test("orderedStat(1) returns minimum") {
    val a = Array(1, 2, 5, 6)

    assert(a.orderedStat(1) == 1)

    val b = Array(3, 6, 2, 5)
    assert(b.orderedStat(1) == 2)

    val c = Array(5)
    assert(c.orderedStat(1) == 5)

    val d = Array(-1, 4, -2, 9, 0, -4, 3)
    assert(d.orderedStat(1) == -4)
  }

  test("orderedStat(n) returns maximum") {
    val a = Array(1, 2, 5, 6)
    assert(a.orderedStat(4) == 6)

    val b = Array(3, 7, 2, 5)
    assert(b.orderedStat(4) == 7)

    val c = Array(5, 2)
    assert(c.orderedStat(2) == 5)

    val d = Array(-1, 4, -2, 9, 0, -4, 3)
    assert(d.orderedStat(7) == 9)
  }

  test("median should return array's median") {
    assert(Array(5).median() == 5)
    assert(Array(3, 2).median() == 2)
    assert(Array(4, 3, 2).median() == 3)
    assert(Array(1, 3, 2).median() == 2)
    assert(Array(4, 3, 2, 5).median() == 3)
    assert(Array(4, 3, 2, 5, 1).median() == 3)
  }
}
