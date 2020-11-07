package scalg.stuctures

import java.util.NoSuchElementException

import Heapable.makeArrayHeapable

import scala.reflect.ClassTag
/**
 *  Implementation of the heap data structure.
 *  Basically it's array-based binary tree, any node of which satisfies max-heap property:
 *            Parent is greater or equal to any of it's children
 *
 *  It support operation of popping max element and creating from array
 */
class FixedSizeHeap[T](heapSize: Int)(implicit ordering: Ordering[T], classTag: ClassTag[T]) extends Heap[T] {

  private val heap: Heapable[T] = makeArrayHeapable(new Array[T](heapSize))
  heap.heapSize = 0

  override def insert(element: T): Unit = {
    heap.heapSize += 1
    update(heap.heapSize - 1, element)
  }

  override def max: Option[T] = if (!isEmpty) Some(heap(0)) else None

  override def isEmpty: Boolean = heap.heapSize == 0

  def extractMax: T = {
    if(isEmpty) {
      throw new NoSuchElementException("Queue is empty")
    } else {
      val el = heap(0)

      heap.swap(0, heap.heapSize - 1)
      heap.heapSize -= 1
      heap.heapify(0)

      el
    }
  }


  private def update(index: Int, value: T): Unit = {
    heap(index) = value

    var i = index
    while (heap.parent(i).isDefined && (ordering.compare(heap(heap.parent(i).get), value) < 0)) {
      heap.swap(i, heap.parent(i).get)
      i = heap.parent(i).get
    }
  }


}


object FixedSizeHeap {

  def apply[T](elements: T*)(implicit ordering: Ordering[T], classTag: ClassTag[T]): Heap[T] = {
    val heap =  new FixedSizeHeap[T](elements.length)
    for(el <- elements)  heap.insert(el)
    heap
  }
}


