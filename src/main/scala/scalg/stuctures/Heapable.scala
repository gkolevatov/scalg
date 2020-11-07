package scalg.stuctures

import scala.annotation.tailrec


/**
 * Enriches array with methods, which help to build a heap, based on that array
 *
 * ==Overview==
 * Heap is a binary tree, each node satisfy the heap ordering property:
 *   - The value of each node is less than or equal to the value of it's parent
 *
 * Tree is packed into array in a following order:
 *  - First element is a tree root
 *  - Left child of nth element has index 2*n + 1 (2*n for 1-indexed array)
 *  - Right child of nth element has index 2*n +2 (2*n + 1 for 1-indexed array)
 *
 * @note This class doesn't create heap from array automatically, it simply provides useful methods,
 * which allows the clients to do it themselves.
 *
 * @param a underlying array to be used as a heap
 */
class Heapable[T] (a: Array[T])(implicit ordering: Ordering[T]) {
  private var _heapSize = a.length
  private val data = a

  def heapSize: Int = _heapSize
  def heapSize_=(value: Int): Unit = {
    if (value < 0) throw new IllegalArgumentException(s"$value is not a valid value for a heap size")
    if (value > a.length) throw new IllegalArgumentException(s"heapSize can't be greater that underlying array size: $value > ${a.length}")

    _heapSize = value
  }


  /**
   * Returns index of the parent of the element with given index or None
   * if there is no element with such index or element is a root
   */
  def parent(i: Int): Option[Int] = if ((i > 0) && (i < heapSize)) Some((i - 1)/2) else None

  /**
   * Returns index of the left child of the element with given index or
   * None if the element has no left child.
   */
  // TODO avoid code repetition here and in right()
  def left(i: Int): Option[Int] = if ((i >= 0) && (2*i + 1 < heapSize)) Some(2*i + 1) else None

  /**
   * Returns index of the left child of the element with given index or
   * None if the element has no left child.
   */
  def right(i: Int): Option[Int] = if ((i >= 0) && (2*i + 2 < heapSize)) Some(2*i + 2) else None


  /**
   * Makes array a heap starting from i index, with assumption, that
   * subtrees of that element are already heaps.
   */
  @tailrec
  final def heapify(i: Int): Unit = {
    if (i >= 0 && i < heapSize) {
      val maxElementIndex = List(Some(i), left(i), right(i)).flatten.maxBy(a(_))

      if (maxElementIndex != i) {
        swap(i, maxElementIndex)
        heapify(maxElementIndex)
      }
    }
  }

  /**
   * Swaps i-th and j-th elements of the array
   * @param i index of the first element to be swapped with the second
   * @param j index of the second element to be swapped with the first
   */
  def swap(i: Int, j: Int): Unit = {
    val c = a(i)
    a(i) = a(j)
    a(j) = c
  }

}

object Heapable {
  /**
   * Create Heapable from an array of some type. The type should support ordering, since heaps are ordered.
   */
  implicit def makeArrayHeapable[T](a: Array[T])(implicit ordering: Ordering[T]): Heapable[T] = new Heapable[T](a)

  implicit def makeHeapableArray[T](heap: Heapable[T]): Array[T] = heap.data

  def makeHeap[T](a: Array[T])(implicit ordering: Ordering[T]): Heapable[T] = {
    val heap: Heapable[T] = a
    for (i <- (a.length / 2) to 0 by -1) heap.heapify(i)

    heap
  }
}