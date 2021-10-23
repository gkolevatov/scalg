package scalg.sorting

import scalg.stuctures.Heapable.makeHeap
import scalg.stuctures.RichArray.makeArrayRich

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random


class OrderableArray[T](a: Array[T])(implicit ordering: Ordering[T], classTag: ClassTag[T]) {
  private lazy val rnd = new Random()

  def heapSort: Array[T] = {
    val underlying = a.clone()
    val heap = makeHeap(underlying)

    while(heap.heapSize > 1) {
      heap.swap(0, heap.heapSize - 1)
      heap.heapSize -= 1
      heap.heapify(0)
    }

    underlying
  }

  /**
   * Returns i-th ordered statistics: the element which has i-1 elements lower or equal to it
   * @param i number of ordered statistics. Count starts from 1
   * @throws IllegalArgumentException if i lower than 1 or greater than array length
   */
  def orderedStat(i: Int): T = {
    if (i < 1 || i > a.length) {
      throw new IllegalArgumentException(s"$i-th Ordered statistic does not exists for array with length ${a.length}")
    }
    orderedStat(0, a.length, i)
  }

  /**
   * Returns median of the array. The element which has the same number of elements greater than it or lower than it.
   * If array length is even, than lower median is taken.
   */
  def median(): T = orderedStat((a.length + 1)/2)

  @tailrec
  private def orderedStat(s: Int, e: Int, i: Int): T = {
    if (e - s == 1) {
      a(s)
    } else {
      val m = partition(s, e)
      val c = m - s + 1
      if (c == i) {
        a(m)
      } else if (c > i) {
        orderedStat(s, m, i)
      } else {
        orderedStat(m + 1, e, i - c)
      }
    }
  }

  private def partition(s: Int, e: Int): Int = {
    val j = rnd.nextInt(e - s)
    a.swap(s + j, e - 1)

    val pivot = a(e - 1)
    var smallerRegionRight = s - 1
    for (i <- s until e) {
      if (ordering.lteq(a(i), pivot)) {
        smallerRegionRight += 1
        a.swap(smallerRegionRight, i)
      }
    }

    smallerRegionRight
  }
}
