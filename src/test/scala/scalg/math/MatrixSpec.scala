package scalg.math

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixSpec extends AnyFunSuite {

  test("Empty matrix is a matrix which has no row and now column") {
    val a = Matrix()

    assert(0 == a.rowCount)
    assert(0 ==a.colCount)
    assert("()" == a.toString)
  }

  test("Empty matrix equals to another empty matrix") {
    assert(Matrix() == Matrix())
  }

  test("A row is a vector of one row") {
    val a = Row(1, 2, 3)
    assert(a(0)(0) == 1)
    assert(a(0)(1) == 2)
    assert(a(0)(2) == 3)

    assert("(1, 2, 3)" == a.toString)
  }

  test("Rows of similar size can be concatenated") {
    val a: Matrix[Int] = Row(1, 2, 3) ::
      Row(3, 4, 5)
    assert(a(0)(0) == 1)
    assert(a(0)(1) == 2)
    assert(a(0)(2) == 3)

    assert(a(1)(0) == 3)
    assert(a(1)(1) == 4)
    assert(a(1)(2) == 5)
    assert("(1, 2, 3)\n(3, 4, 5)".trim == a.toString)
  }

  test("A column is a vector of one column") {
    val a = Column(1, 2, 3)
    assert(1 == a(0)(0))
    assert(2 == a(1)(0))
    assert(3 == a(2)(0))

    assert("(1)\n(2)\n(3)" == a.toString)
  }

  test("Equal matrices it's matrix containing same elements") {
    val a = Row(1, 2, 3) ::
      Row(3, 4, 5) ::
      Row(5, 6, 7)

    val b = Row(1, 2, 3) ::
      Row(3, 4, 5) ::
      Row(5, 6, 7)

    assert(a == b)
  }

  test("if matrices has one different element they are unequal") {
    val a = Row(1, 2, 3) ::
      Row(3, 8, 5) ::
      Row(5, 6, 7)

    val b = Row(1, 2, 3) ::
      Row(3, 4, 5) ::
      Row(5, 6, 7)

    assert(a != b)
  }

  test("Matrices with different row count cannot be compares") {
    val a = Row(1, 2, 3) ::
      Row(3, 8, 5)

    val b = Row(1, 2, 3)

    assertThrows[IllegalArgumentException] {
      a == b
    }
  }

  test("Matrices with different column count cannot be compared") {
    val a = Row(1, 2, 3)

    val b = Row(1, 2)

    assertThrows[IllegalArgumentException] {
      a == b
    }
  }


  test("Matrices can be summed up") {

    val a: Matrix[Int] = Row(1, 2, 3) ::
      Row(4, 5, 6) ::
      Row(7, 8, 9)

    val b: Matrix[Int] = Row(1, 2, 3) ::
      Row(4, 5, 6) ::
      Row(7, 8, 9)

    val c = Row(2, 4, 6) ::
      Row(8, 10, 12) ::
      Row(14, 16, 18)

    assert(c == a + b)
  }

  test("An element can be added to a Matrix") {
    val a: Matrix[Int] = Row(1, 2, 3) ::
      Row(4, 5, 6) ::
      Row(7, 8, 9)

    val b = Row(6, 7, 8) ::
      Row(9, 10, 11) ::
      Row(12, 13, 14)

    assert(b == (a + 5))
  }

  test("Matrix can be subtracted") {
    val a: Matrix[Int] = Row(1, 2, 3) ::
      Row(4, 5, 6) ::
      Row(7, 8, 9)

    val b = Row(0, 0, 0) ::
      Row(0, 0, 0) ::
      Row(0, 0, 0)

    assert(b == (a - a))
  }

  test("An element can be subtracted from matrix") {
    val a: Matrix[Int] = Row(1, 2, 3) ::
      Row(4, 5, 6) ::
      Row(7, 8, 9)

    val b = Row(0, 0, 0) ::
      Row(0, 0, 0) ::
      Row(0, 0, 0)

    assert(b == (a - a))
  }

  test("Matrix can be multiplied by an element") {
    val a = Row(1, 2, 3) ::
      Row(4, 5, 6) ::
      Row(7, 8, 9)

    val b = Row(2, 4, 6) ::
      Row(8, 10, 12) ::
      Row(14, 16, 18)

    assert(b == a * 2)
  }

  test("Matrix can be multiplied by a matrix if first matrix rowCount matches second matrix colCount") {
    val a = Row(1, 2, 3) ::
      Row(4, 5, 6)

    val b = Row(1, 2) ::
      Row(3, 4) ::
      Row(5, 6)

    val c = Row(22, 28) ::
      Row(49, 64)

    assert(c == a*b)
  }

}

