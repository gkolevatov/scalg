package scalg.seq

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import SeqAlgorithm.maxSubArraySum

@RunWith(classOf[JUnitRunner])
class SeqAlgorithmTest extends AnyFunSuite {

  test("test empty array") {
    assertThrows[UnsupportedOperationException](maxSubArraySum(Array.empty[Int]))
  }

  test("test one-element array") {
    assert(maxSubArraySum(Array(1)) == 1)
    assert(maxSubArraySum(Array(0)) == 0)
    assert(maxSubArraySum(Array(-1)) == -1)
  }

  test("test all-positive array") {
    assert(maxSubArraySum(Array(1, 2, 3, 4, 5)) == 15)
  }

  test("test all-negative-order array") {
    assert(maxSubArraySum(Array(-5, -4, -3, -2, -1)) == -1)
  }

  test("test left border array") {
    assert(maxSubArraySum(Array(4, 5, -1, -2, -3)) == 9)
    assert(maxSubArraySum(Array(5, -1, -2)) == 5)
    assert(maxSubArraySum(Array(3, 4, -1, -2)) == 7)
  }

  test("test center border array") {
    assert(maxSubArraySum(Array(1, 4, 5, -3, -2)) == 10)
    assert(maxSubArraySum(Array(-1, -4, 5, 6, 4, -5)) == 15)
  }

  test("test right border array") {
    assert(maxSubArraySum(Array(-1, -3, 2, 5, 6)) == 13)
    assert(maxSubArraySum(Array(-6, -5, 1, 2, 3, 4, 7)) == 17)
  }

}
