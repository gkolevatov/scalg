package scalg.stuctures

/**
 *  Heap data structure.
 *  Allows insertion and retrieving max element.
 *
 * ==Performance==
 * '''Time:'''
 *   - insert is effectively O(ln(n) operation
 *   - max is O(1)
 *   - extractMax is effectively O(ln(n) operation
 * '''Space:'''
 *  Heap consumes O(n) memory without overheads
 *
 */
trait Heap[T] {

  /**
   * Inserts element in the heap
   * @throws IllegalStateException if it's impossible to add new element
   * @param element element to be inserted
   */
  def insert(element: T): Unit

  /**
   * Get max element without removing it
   * If there's no such element, e. g. the heap is empty, returns None   *
   *
   * @return max heap element
   */
  def max: Option[T]

  /**
   * Retrieves max element and removes it from the heap
   * If there's no such element, e. g. the heap is empty, an exception is thrown
   *
   * @throws NoSuchElementException if it's impossible to retrieve an element
   * @return max heap element
   */
  def extractMax: T

  /**
   * Returns if heap is empty
   *
   */
  def isEmpty: Boolean
}
