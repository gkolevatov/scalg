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
}
