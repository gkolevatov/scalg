package scalg.math

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MathTest extends AnyFunSuite {

  test("Math.sqrt should return square roots for perfect square numbers") {
    for(i <- 0 until math.sqrt(Int.MaxValue).toInt) {
      assert(Math.sqrt(i*i) == i)
    }
  }

  test("Math.sqrt should fail for every negative value") {
    assertThrows[IllegalArgumentException]{Math.sqrt(-1)}
    assertThrows[IllegalArgumentException]{Math.sqrt(-2)}
    assertThrows[IllegalArgumentException]{Math.sqrt(-500)}
    assertThrows[IllegalArgumentException]{Math.sqrt(Int.MinValue)}
  }

  test("Math.sqrt should fail for every number which is not perfect square number") {
    assertThrows[IllegalArgumentException]{Math.sqrt(2)}
    assertThrows[IllegalArgumentException]{Math.sqrt(101)}
    assertThrows[IllegalArgumentException]{Math.sqrt(Int.MaxValue)}
  }

  test("Math.sum should return None for empty params, or if only None params given") {
    assert(Math.sum[Int]().isEmpty)
    assert(Math.sum[Int](None, None, None).isEmpty)
  }

  test("Math.sum should return sum of existing params") {
    assert(Math.sum(None, Some(1)).get == 1)
    assert(Math.sum(None, Some(1), Some(2), None, Some(1)).get == 4)
    assert(Math.sum(Some(1), Some(2), Some(3)).get == 6)
  }


  test("Math.max should return None for empty params, or if only None params given") {
    assert(Math.max[Int]().isEmpty)
    assert(Math.max[Int](None, None, None).isEmpty)
  }

  test("Math.max should return maximum value of given non empty values") {
    assert(Math.max(None, Some(1)).get == 1)
    assert(Math.max(None, Some(1), Some(2), None, Some(1)).get == 2)
    assert(Math.max(Some(1), Some(3), Some(2)).get == 3)
  }
}
