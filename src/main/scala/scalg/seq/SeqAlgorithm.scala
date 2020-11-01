package scalg.seq


import scala.reflect.ClassTag
import scalg.math.Math.max
import scalg.math.Math.sum

/**
 * The object contains implementations of basic algorithm which manipulates with sequence structures such as Arrays and Lists
 */
object SeqAlgorithm {

  /**
   *  Finds subarray containing with the max possible sum of elements and returns this sum.
   *  If given array is empty exception is thrown
   *
   * @param elements Array of elements. Should not be empty
   * @return Max subarray sum
   */
  def maxSubArraySum[T](elements: Array[T])(implicit numeric: Numeric[T], classTag: ClassTag[T]): T = {
    if (elements.isEmpty) {
      throw new UnsupportedOperationException("maxSubArraySum(empty)")
    } else if (elements.length == 1) {
      elements(0)
    } else {
      val median = elements.length / 2

      val left = elements.slice(0, median)
      val right = elements.slice(median, elements.length)

      val leftSum = if (!left.isEmpty) Some(maxSubArraySum(left)) else None
      val rightSum = if (!right.isEmpty) Some(maxSubArraySum(right)) else None

      def maxSerialSum(a: Array[T]): Option[T] = {
        if (a.isEmpty) {
          None
        } else {
          var tmp = a(0)
          var result = tmp

          for (i <- 1 until a.length) {
            tmp = numeric.plus(tmp, a(i))
            if (numeric.compare(tmp, result) > 0) {
              result = tmp
            }
          }

          Some(result)
        }
      }

      val leftPartSum = maxSerialSum(left.reverse)
      val rightPartSum = maxSerialSum(right)


      max(leftSum, rightSum, sum(leftPartSum, rightPartSum)).get
    }
  }


}
