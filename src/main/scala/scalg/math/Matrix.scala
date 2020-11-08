package scalg.math

import scala.reflect.ClassTag

object Matrix {
  private val EMPTY: Matrix[Nothing] = new Matrix[Nothing](0, 0, Array())

  /**
   * Creates empty matrix
   * @return Empty matrix
   */
  def apply(): Matrix[Nothing] = EMPTY


  /**
   * Creates 2d Matrix with given size and fills it with given lambda
   * @param rowCount Number of rows
   * @param colCount Number of columns
   * @param elem Function calculated element number by it's indexes
   * @return 2-dimension Matrix
   */
  def fill[T](rowCount: Int, colCount: Int)(elem: (Int, Int) => T)(implicit tag: ClassTag[T]): Matrix[T] = {
    new Matrix[T](rowCount, colCount, Array.tabulate(rowCount)((i: Int) => {
      Array.tabulate(colCount) {
        elem(i, _)
      }
    }))
  }

  def assertSameRowCount[T](a: Matrix[T], b: Matrix[T]): Unit = {
    if (a.rowCount != b.rowCount) {
      throw new IllegalArgumentException("Matrix " + a.matrixSize + " is incompatible with matrix " + a.matrixSize + " should have the same row count")
    }
  }

  def assertSameColumnCount[T](a: Matrix[T], b: Matrix[T]): Unit = {
    if (a.colCount != b.colCount) {
      throw new IllegalArgumentException("Matrix " + a.matrixSize + " is incompatible with matrix " + a.matrixSize + " should have the same column count")
    }
  }

  def assertSameSize[T](a: Matrix[T], b: Matrix[T]): Unit = {
    assertSameRowCount(a, b)
    assertSameColumnCount(a, b)
  }

}

/**
 * Row vector representing a matrix of one row.
 * Can be instantiated from sequence of elements.
 */
object Row {
  def apply[T](elements: T*)(implicit tag: ClassTag[T]): Matrix[T] = {
    Matrix.fill[T](1, elements.length)((_, j) => elements(j))
  }
}

/**
 * Column vector representing a matrix of one column.
 * Can be instantiated from sequence of elements.
 */
object Column {
  def apply[T](elements: T*)(implicit tag: ClassTag[T]): Matrix[T] = {
    Matrix.fill[T](elements.length, 1)((i, _) => elements(i))
  }
}

/**
 * Squared matrix.
 * Can be instantiated from sequence of elements. Number of elements should be a perfect square.
 */
object Square {
  def apply[T](elements: T*)(implicit tag: ClassTag[T]): Matrix[T] = {
    val size: Int = Math.sqrt(elements.length)

    Matrix.fill[T](size, size)((i, j) => {
      elements(i*size + j)
    })
  }
}

/**
 * Represents 2-dimensional matrix
 */
class Matrix[T] private(val rowCount: Int, val colCount: Int, private val data: Array[Array[T]]) {

  def apply(i: Int)(j: Int): T = {
    data(i)(j)
  }

  override def toString: String = {
    if (rowCount != 0) {
      data.foldLeft("") {
        _ + _.mkString("(", ", ", ")\n")
      }.trim
    } else {
      "()"
    }
  }

  private def matrixSize: String = {
    "[" + rowCount + "x" + colCount + "]"
  }

  def ::(matrix: Matrix[T])(implicit tag: ClassTag[T]): Matrix[T] = {
    if (this.colCount == matrix.colCount) {
      Matrix.fill(matrix.rowCount + this.rowCount, matrix.colCount)((i, j) => {
        if (i < matrix.rowCount) matrix(i)(j) else this (i - matrix.rowCount)(j)
      })
    } else if (this.rowCount == matrix.rowCount) {
      Matrix.fill(matrix.rowCount, matrix.colCount + this.colCount)((i, j) => {
        if (j < matrix.colCount) matrix(i)(j) else this (i)(j - matrix.colCount)
      })
    } else {
      throw new IllegalArgumentException("Matrix " + matrix.matrixSize + " is incompatible with matrix " + this.matrixSize)
    }
  }

  def +(matrix: Matrix[T])(implicit tag: ClassTag[T], num: Numeric[T]): Matrix[T] = {
    Matrix.assertSameSize(this, matrix)

    Matrix.fill(this.rowCount, this.colCount)((i, j) => {
      num.plus(this(i)(j), matrix(i)(j))
    })
  }

  def -(matrix: Matrix[T])(implicit tag: ClassTag[T], num: Numeric[T]): Matrix[T] = {
    Matrix.assertSameSize(this, matrix)

    Matrix.fill(this.rowCount, this.colCount)((i, j) => {
      num.minus(this(i)(j), matrix(i)(j))
    })
  }

  def +(element: T)(implicit tag: ClassTag[T], num: Numeric[T]): Matrix[T] = {
    Matrix.fill(this.rowCount, this.colCount)((i, j) => {
      num.plus(this(i)(j),element)
    })
  }

  def -(element: T)(implicit tag: ClassTag[T], num: Numeric[T]): Matrix[T] = {
    Matrix.fill(this.rowCount, this.colCount)((i, j) => {
      num.minus(this(i)(j),element)
    })
  }

  def *(element: T)(implicit tag: ClassTag[T], num: Numeric[T]): Matrix[T] = {
    Matrix.fill(this.rowCount, this.colCount)((i, j) => {
      num.times(this(i)(j),element)
    })
  }

  def *(matrix: Matrix[T])(implicit tag: ClassTag[T], num: Numeric[T]): Matrix[T] = {
    if (this.colCount != matrix.rowCount) {
      throw new IllegalArgumentException("Matrix " + this.matrixSize + " cannot be multiplied with matrix " + this.matrixSize)
    }

    Matrix.fill(this.rowCount, matrix.colCount)((i, j) => {
      (0 until this.colCount).foldLeft(num.zero)((r, k) => {
        num.plus(r, num.times(this(i)(k), matrix(k)(j)))
      })
    })
  }


  def ==(matrix: Matrix[T]): Boolean = {
    Matrix.assertSameSize(this, matrix)

    !(0 until this.rowCount).exists(i =>
      (0 until this.rowCount).exists(j =>
        this (i)(j) != matrix(i)(j)
      )
    )
  }

}
