package scalg.math

object Math {

  /**
   * Returns square root for Int value.
   * If value is not perfect square number it fails with IllegalArgumentException
   * @param x the number to take the square root of
   * @return the square root of the input value
   * @group root-extraction
   */
  def sqrt(x: Int): Int = {
    if (x < 0)
      throw new IllegalArgumentException(x + " is not a perfect square number")

    val tst = (scala.math.sqrt(x) + 0.5).toInt
    if (tst * tst != x)
      throw new IllegalArgumentException(x + " is not a perfect square number")

    tst
  }
}
