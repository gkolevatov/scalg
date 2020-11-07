package scalg.sorting

import scalg.stuctures.Heapable.makeHeap

import scala.reflect.ClassTag

class SortableArray[T](a: Array[T])(implicit ordering: Ordering[T], classTag: ClassTag[T]) {
  def heapSort: Array[T] = {
    val heap = makeHeap(a)

    while(heap.heapSize > 1) {
      heap.swap(0, heap.heapSize - 1)
      heap.heapSize -= 1
      heap.heapify(0)
    }

    a
  }
}
