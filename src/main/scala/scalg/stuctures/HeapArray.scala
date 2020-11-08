package scalg.stuctures

import java.util.NoSuchElementException

import Heapable.makeArrayHeapable
import Heapable.makeHeapableArray
import scala.reflect.ClassTag

/**
 *  Implementation of the heap data structure.
 *  Basically it's array-based binary tree, any node of which satisfies max-heap property:
 *            Parent is greater or equal to any of it's children
 *
 *  It support operation of popping max element and creating from array
 */
class HeapArray[T](heapSize: Int)(implicit ordering: Ordering[T], classTag: ClassTag[T]) extends Heap[T] {

  private var heap: Heapable[T] = makeArrayHeapable(new Array[T](heapSize))
  heap.heapSize = 0

  override def insert(element: T): Unit = {
    if (heap.heapSize == heap.length) {
      assignAsNewHeap(new Array[T](2*heap.length))
    }

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

      if (heap.heapSize < heap.length/2 && heap.heapSize > heapSize) {
        assignAsNewHeap(new Array[T](heap.length / 2))
      }

      el
    }
  }

  private def assignAsNewHeap(newHeap: Heapable[T]) = {
    Array.copy(makeHeapableArray(heap), 0, makeHeapableArray(newHeap), 0, heap.heapSize)
    newHeap.heapSize = heap.heapSize
    heap = newHeap
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


object HeapArray {

  def apply[T](elements: T*)(implicit ordering: Ordering[T], classTag: ClassTag[T]): HeapArray[T] = {
    val heap =  new HeapArray[T](elements.length)
    for(el <- elements)  heap.insert(el)
    heap
  }
}


