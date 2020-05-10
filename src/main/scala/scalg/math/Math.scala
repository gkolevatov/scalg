package scalg.math

/**
 * Object contains methods for performing some mathematical operations which does not exist in scala.math object.
 */
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

  /**
   * Calculate sum of optional values. If there is no values returns None
   * @param values sequence of optional values
   * @return sum of existing values. None if there is no such values.
   */
  def sum[T](values: Option[T]*)(implicit numeric: Numeric[T]): Option[T] = {
    val nonEmptyList = values
      .filter(_.nonEmpty)
      .map(_.get)

    if (nonEmptyList.isEmpty) {
      None
    } else {
      Some(nonEmptyList.sum)
    }
  }

  /**
   * Calculate maximum of optional values. If there is no values returns None
   * @param values sequence of optional values
   * @return maximum of existing values. None if there is no such values.
   */
  def max[T](values: Option[T]*)(implicit numeric: Numeric[T]): Option[T] = {
    values
      .filter(_.nonEmpty)
      .map(_.get)
      .maxOption
  }
}
