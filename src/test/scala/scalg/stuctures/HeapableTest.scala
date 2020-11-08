package scalg.stuctures

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import _root_.scalg.stuctures.Heapable.makeArrayHeapable


@RunWith(classOf[JUnitRunner])
class HeapableTest extends AnyFunSuite {
  test("parent(x) should return x/2 if x is a part of the heap and not root") {
    val a = Array(1, 2, 3, 4, 5)
    assert(a.parent(1).get == 0)
    assert(a.parent(2).get == 0)
    assert(a.parent(3).get == 1)
    assert(a.parent(4).get == 1)
  }

  test("parent(x) should return None if index is not a part of the heap") {
    val a = Array(1, 2, 3, 4, 5)
    assert(a.parent(-1).isEmpty)
    assert(a.parent(6).isEmpty)
    assert(a.parent(10).isEmpty)
  }

  test("parent(x) should return None if index is a root") {
    assert(Array(1, 2, 3, 4, 5).parent(0).isEmpty)
    assert(Array(2).parent(0).isEmpty)
  }

  test("left(x) should return 2*x + 1 if x - part of the heap") {
    val a = Array(1, 2, 3, 4, 5)
    assert(a.left(0).get == 1)
    assert(a.left(1).get == 3)
  }

  test("left(x) should return None if index is not part of the heap or has no lef child") {
    val a = Array(1, 2, 3, 4, 5)
    assert(a.left(5).isEmpty)
    assert(a.left(4).isEmpty)
    assert(a.left(3).isEmpty)
    assert(a.left(2).isEmpty)
    assert(a.left(-1).isEmpty)
  }

  test("right(x) should return 2*x + 2 if x - part of the heap") {
    val a = Array(1, 2, 3, 4, 5)
    assert(a.right(0).get == 2)
    assert(a.right(1).get == 4)
  }

  test("right(x) should return None if x - not part of the heap or has no lef child")  {
    val a = Array(1, 2, 3, 4, 5)
    assert(a.right(5).isEmpty)
    assert(a.right(4).isEmpty)
    assert(a.right(3).isEmpty)
    assert(a.right(2).isEmpty)
    assert(a.right(-1).isEmpty)
  }

  test("heapify(i) on one-element array should done nothing") {
    val a = Array(1)
    a.heapify(0)
    assert(a sameElements Array(1))
  }

  test("heapify(i) called on heap leaf should done nothing") {
    val a = Array(1, 2, 3, 4, 5, 6, 7)

    a.heapify(3)
    a.heapify(4)
    a.heapify(5)
    a.heapify(6)
    assert(a sameElements  Array(1, 2, 3, 4, 5, 6, 7))
  }

  test("heapify(i) should make heap of two- and three-element arrays") {
    val a = Array(1, 2, 3)
    a.heapify(0)

    assert(a sameElements Array(3, 2, 1))

    val b = Array(1, 2)
    b.heapify(0)

    assert(b sameElements Array(2, 1))
  }

  test("heapify should done nothing to a heap") {
    val heap = Array(8, 3, 7, 1, 2, 5, 6)
    heap.heapify(0)
    assert(heap sameElements Array(8, 3, 7, 1, 2, 5, 6))

    heap.heapify(1)
    assert(heap sameElements Array(8, 3, 7, 1, 2, 5, 6))

    heap.heapify(2)
    assert(heap sameElements Array(8, 3, 7, 1, 2, 5, 6))

    heap.heapify(3)
    assert(heap sameElements Array(8, 3, 7, 1, 2, 5, 6))

    heap.heapify(4)
    assert(heap sameElements Array(8, 3, 7, 1, 2, 5, 6))
  }

  test("heapify should change swap specified element and the greater of its children") {
    val a = Array(7, 3, 8, 1, 2, 5, 6)

    a.heapify(0)
    assert(a sameElements Array(8, 3, 7, 1, 2, 5, 6))
  }

  test("heapify should push specified element down to the tree of greater children, if heap property wasn't satisfied on first step") {
    val a = Array(4, 3, 7, 1, 2, 5, 6)

    a.heapify(0)
    assert(a sameElements Array(7, 3, 6, 1, 2, 5, 4))
  }

  test("heapSize returns size of current heap. By default whole array is considered to be a heap") {
    val a = Array(4, 3, 7, 1, 2, 5, 6)
    assert(a.heapSize == 7)
  }

  test("heapSize could be changed") {
    val a: Heapable[Int] = Array(4, 3, 7, 1, 2, 5, 6)
    a.heapSize = 2
    assert(a.heapSize == 2)

    a.heapSize = 5
    assert(a.heapSize == 5)
  }

  test("heapSize could be less than zero and greater than underlying array size") {
    val a = Array(4, 3, 7, 1, 2, 5, 6)
    assertThrows[IllegalArgumentException]({a.heapSize = -1})
    assertThrows[IllegalArgumentException]({a.heapSize = 10})
  }

  test("heapSize specify which part of the array is considered to be a heap") {
    val a = Array(1, 4, 2, 3)

    assert(a.left(1).get == 3)

    a.heapify(0)
    assert(a sameElements Array(4, 3, 2, 1))

    val b = Array(1, 4, 2, 3)
    val heapable: Heapable[Int] = b
    heapable.heapSize = 3
    heapable.heapify(0)

    assert(heapable.right(0).isDefined)
    assert(heapable.left(1).isEmpty)
    assert(b sameElements Array(4, 1, 2, 3))

    heapable.heapSize = 2
    assert(heapable.right(0).isEmpty)
  }
}
