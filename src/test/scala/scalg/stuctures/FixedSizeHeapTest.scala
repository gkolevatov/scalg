package scalg.stuctures

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FixedSizeHeapTest extends AnyFunSuite {
  test("isEmptFixedSizeHeap()") {
    assert(FixedSizeHeap[Int]().isEmpty)
    assert(!FixedSizeHeap(1, 2, 3).isEmpty)
  }

  test("heap allows retrieving max element") {
    val heap = FixedSizeHeap(1, 2, 3)

    assert(heap.max.get == 3)
    assert(heap.max.get == 3)
    assert(heap.max.get == 3)
  }

  test("heap allows retrieving max element and removing it from the queue") {
    val heap = FixedSizeHeap(1, 2, 3)
    assert(heap.extractMax == 3)
    assert(heap.extractMax == 2)
    assert(heap.extractMax == 1)
    assert(heap.isEmpty)
  }

  test("if heap is empty max() returns None") {
    assert(FixedSizeHeap[Int]().max.isEmpty)

    val heap = FixedSizeHeap(1, 2, 3)
    assert(heap.max.get == 3)
    assert(heap.extractMax == 3)

    assert(heap.max.get == 2)
    assert(heap.extractMax == 2)

    assert(heap.max.get == 1)
    assert(heap.extractMax == 1)
    assert(heap.max.isEmpty)
  }

  test("if heap is empty extractMax throws NoSuchElementException") {
    assertThrows[NoSuchElementException] { FixedSizeHeap[Int]().extractMax  }

    val heap = FixedSizeHeap(1, 2, 3)
    heap.extractMax
    heap.extractMax
    heap.extractMax

    assert(heap.isEmpty)
    assertThrows[NoSuchElementException] { heap.extractMax  }
  }

  test("if insert add new element to the heap") {
    val heap = new FixedSizeHeap[Int](3)
    heap.insert(1)
    heap.insert(10)
    heap.insert(5)

    assert(heap.max.get == 10)
    assert(heap.extractMax == 10)

    heap.insert(4)
    assert(heap.max.get == 5)
    assert(heap.extractMax == 5)

    assert(heap.max.get == 4)
    assert(heap.extractMax == 4)

    heap.insert(8)
    heap.insert(6)

    assert(heap.max.get == 8)
    assert(heap.extractMax == 8)

    assert(heap.max.get == 6)
    assert(heap.extractMax == 6)

    assert(heap.max.get == 1)
    assert(heap.extractMax == 1)

    assert(heap.isEmpty)
  }


}
