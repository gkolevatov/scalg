package scalg.stuctures

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import scalg.stuctures.trees.BinarySearchTree

@RunWith(classOf[JUnitRunner])
class BinarySearchTreeTest extends AnyFunSuite {
  test("tree should be traversable in natural order") {
    val tree = new BinarySearchTree[Int]()

    tree.insert(10)
    tree.insert(5)
    tree.insert(8)
    tree.insert(1)

    assert(tree.toList == List(1, 5, 8, 10))

    tree.insert(4)
    tree.insert(3)
    tree.insert(7)
    tree.insert(2)
    tree.insert(6)
    tree.insert(9)
    assert(tree.toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  test("empty tree should be traversable") {
    val tree = new BinarySearchTree[Int]()
    tree.foreach { _ => assert(false) }
  }

  test("inserting already existing element shouldn't affect the tree") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(5)
    tree.insert(4)
    tree.insert(6)

    assert(tree.toList == List(4, 5, 6))

    tree.insert(6)
    assert(tree.toList == List(4, 5, 6))

    tree.insert(5)
    assert(tree.toList == List(4, 5, 6))

    tree.insert(4)
    assert(tree.toList == List(4, 5, 6))

    tree.insert(1)
    tree.insert(2)
    tree.insert(3)

    assert(tree.toList == List(1, 2, 3, 4, 5, 6))

    tree.insert(1)
    tree.insert(2)
    tree.insert(3)

    assert(tree.toList == List(1, 2, 3, 4, 5, 6))

    tree.insert(9)
    tree.insert(7)
    tree.insert(8)

    assert(tree.toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9))

    tree.insert(9)
    tree.insert(7)
    tree.insert(8)

    assert(tree.toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("insert should return inserted node") {
    val tree = new BinarySearchTree[Int]()
    val root = tree.insert(2)
    val leftchild = tree.insert(1)
    val rightchild = tree.insert(3)


    assert(root.value == 2)
    assert(root.left.contains(leftchild))
    assert(root.right.contains(rightchild))

    assert(leftchild.value == 1)
    assert(rightchild.value == 3)
  }

  test("inserting already existed key should return its node") {
    val tree = new BinarySearchTree[Int]()
    val root = tree.insert(2)
    val leftChild = tree.insert(1)
    val rightChild = tree.insert(3)

    val newRoot = tree.insert(2)
    assert(root == newRoot)

    val newLeftChild = tree.insert(1)
    assert(leftChild == newLeftChild)

    val newRightChild = tree.insert(3)
    assert(rightChild == newRightChild)
  }

  test("max and min should return max and min node in the tree") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(10)
    tree.insert(5)
    tree.insert(4)
    tree.insert(3)
    tree.insert(2)
    tree.insert(6)
    tree.insert(8)
    tree.insert(1)
    tree.insert(7)
    tree.insert(9)

    assert(tree.maxNode.get.value == 10)
    assert(tree.minNode.get.value == 1)
  }

  test("if tree is empty max and min should return None") {
    val tree = new BinarySearchTree[Int]()
    assert(tree.maxNode.isEmpty)
    assert(tree.minNode.isEmpty)
  }

  test("if tree is has only one element max and min should be the same") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(1)
    assert(tree.maxNode.get == tree.minNode.get)
  }

  test("search should return the node with given key") {
    val tree = new BinarySearchTree[Int]()
    val ten = tree.insert(10)
    val five = tree.insert(5)
    val four = tree.insert(4)
    val three = tree.insert(3)
    val two = tree.insert(2)
    val six = tree.insert(6)
    val eight = tree.insert(8)
    val one = tree.insert(1)
    val seven = tree.insert(7)
    val nine = tree.insert(9)

    assert(ten == tree.search(10).get)
    assert(nine == tree.search(9).get)
    assert(eight == tree.search(8).get)
    assert(seven == tree.search(7).get)
    assert(six == tree.search(6).get)
    assert(five == tree.search(5).get)
    assert(four == tree.search(4).get)
    assert(three == tree.search(3).get)
    assert(two == tree.search(2).get)
    assert(one == tree.search(1).get)
  }

  test("predecessor should return the closest node that lower than given node") {
    val tree = new BinarySearchTree[Int]()
    val ten = tree.insert(10)
    val five = tree.insert(5)
    val four = tree.insert(4)
    val three = tree.insert(3)
    val two = tree.insert(2)
    val six = tree.insert(6)
    val eight = tree.insert(8)
    val one = tree.insert(1)
    val seven = tree.insert(7)
    val nine = tree.insert(9)

    assert(tree.predecessor(four).get.value == 3)
    assert(tree.predecessor(five).get.value == 4)
    assert(tree.predecessor(six).get.value == 5)
    assert(tree.predecessor(seven).get.value == 6)
    assert(tree.predecessor(eight).get.value == 7)
    assert(tree.predecessor(nine).get.value == 8)
    assert(tree.predecessor(ten).get.value == 9)
    assert(tree.predecessor(three).get.value == 2)
    assert(tree.predecessor(two).get.value == 1)
  }

  test("predecessor should return None if there is no predecessor") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(10)
    tree.insert(5)
    tree.insert(4)
    tree.insert(3)
    tree.insert(2)
    tree.insert(6)
    tree.insert(8)
    val one = tree.insert(1)
    tree.insert(7)
    tree.insert(9)

    assert(tree.predecessor(one).isEmpty)
  }

  test("successor should return the closest node that lower than given node") {
    val tree = new BinarySearchTree[Int]()
    val ten = tree.insert(10)
    val five = tree.insert(5)
    val four = tree.insert(4)
    val three = tree.insert(3)
    val two = tree.insert(2)
    val six = tree.insert(6)
    val eight = tree.insert(8)
    val one = tree.insert(1)
    val seven = tree.insert(7)
    val nine = tree.insert(9)

    assert(tree.successor(four).get.value == 5)
    assert(tree.successor(five).get.value == 6)
    assert(tree.successor(six).get.value == 7)
    assert(tree.successor(seven).get.value == 8)
    assert(tree.successor(eight).get.value == 9)
    assert(tree.successor(nine).get.value == 10)
    assert(tree.successor(three).get.value == 4)
    assert(tree.successor(two).get.value == 3)
    assert(tree.successor(one).get.value == 2)
  }

  test("successor should return None if there is no successor") {
    val tree = new BinarySearchTree[Int]()
    val ten = tree.insert(10)
    tree.insert(5)
    tree.insert(4)
    tree.insert(3)
    tree.insert(2)
    tree.insert(6)
    tree.insert(8)
    tree.insert(1)
    tree.insert(7)
    tree.insert(9)

    assert(tree.successor(ten).isEmpty)
  }

  test("delete must delete element from the tree") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(3)
    tree.insert(10)
    tree.insert(5)
    tree.insert(8)
    tree.insert(9)
    tree.insert(1)
    tree.insert(2)
    tree.insert(4)
    tree.insert(7)
    tree.insert(6)

    tree.removeNode(tree.search(4).get)

    assert(tree.toList == List(1, 2, 3, 5, 6, 7, 8, 9, 10))

    tree.removeNode(tree.search(7).get)
    assert(tree.toList == List(1, 2, 3, 5, 6, 8, 9, 10))

    tree.removeNode(tree.search(2).get)
    assert(tree.toList == List(1, 3, 5, 6, 8, 9, 10))
  }

  test("delete must delete max and min element correctly") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(3)
    tree.insert(10)
    tree.insert(5)
    tree.insert(8)
    tree.insert(9)
    tree.insert(1)
    tree.insert(2)
    tree.insert(4)
    tree.insert(7)
    tree.insert(6)

    tree.removeNode(tree.search(10).get)
    tree.removeNode(tree.search(1).get)
    assert(tree.toList == List(2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("delete must delete root min element correctly") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(3)
    tree.insert(10)
    tree.insert(5)
    tree.insert(8)
    tree.insert(9)
    tree.insert(1)
    tree.insert(2)
    tree.insert(4)
    tree.insert(7)
    tree.insert(6)

    tree.removeNode(tree.search(3).get)
    assert(tree.toList == List(1, 2,  4, 5, 6, 7, 8, 9, 10))

    val anotherTree = new BinarySearchTree[Int]()
    anotherTree.insert(2)
    anotherTree.insert(1)
    anotherTree.insert(3)

    anotherTree.removeNode(anotherTree.search(2).get)
    assert(anotherTree.toList == List(1, 3))
  }

  test("delete from single element tree should make tree empty") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(1)
    tree.removeNode(tree.search(1).get)

    assert(tree.toList.isEmpty)
  }

  test("delete from two element tree should work correctly") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(1)
    tree.insert(2)
    tree.removeNode(tree.search(2).get)

    assert(tree.toList == List(1))

    val anotherTree = new BinarySearchTree[Int]()
    anotherTree.insert(1)
    anotherTree.insert(2)

    anotherTree.removeNode(anotherTree.search(1).get)
    assert(anotherTree.toList == List(2))

    val reversedTree = new BinarySearchTree[Int]()
    reversedTree.insert(2)
    reversedTree.insert(1)

    reversedTree.removeNode(reversedTree.search(2).get)

    assert(reversedTree.toList == List(1))

    val anotherReversedTree = new BinarySearchTree[Int]()
    anotherReversedTree.insert(2)
    anotherReversedTree.insert(1)

    anotherReversedTree.removeNode(anotherReversedTree.search(1).get)

    assert(anotherReversedTree.toList == List(2))
  }

  test("delete child from balanced two-leveled treed should work correctly") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(2)
    tree.insert(1)
    tree.insert(3)

    tree.removeNode(tree.search(1).get)
    assert(tree.toList == List(2, 3))
    assert(tree.search(1).isEmpty)
    val root = tree.search(2)
    val child = tree.search(3)
    assert(root.isDefined)
    assert(child.isDefined)
    assert(root.get.right.contains(child.get))
    assert(root.get.left.isEmpty)

    val anotherTree = new BinarySearchTree[Int]()
    anotherTree.insert(2)
    anotherTree.insert(1)
    anotherTree.insert(3)

    anotherTree.removeNode(anotherTree.search(3).get)
    assert(anotherTree.toList == List(1, 2))
    assert(anotherTree.search(3).isEmpty)
    val anotherRoot = anotherTree.search(2)
    val anotherChild = anotherTree.search(1)
    assert(anotherRoot.isDefined)
    assert(anotherChild.isDefined)
    assert(anotherRoot.get.left.contains(anotherChild.get))
    assert(anotherRoot.get.right.isEmpty)
  }

  test("delete root from balanced two-leveled tree  should replace root with right child") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(2)
    tree.insert(1)
    tree.insert(3)

    tree.removeNode(tree.search(2).get)
    assert(tree.toList == List(1, 3))
    assert(tree.search(2).isEmpty)

    val root = tree.search(3)
    val child = tree.search(1)
    assert(root.isDefined)
    assert(child.isDefined)
    assert(root.get.left.contains(child.get))
    assert(root.get.right.isEmpty)
  }

  test("delete replace deleted node with it's right child if it's deleted node's successor") {
    val tree = new BinarySearchTree[Int]()
    val root = tree.insert(2)
    val one = tree.insert(1)
    tree.insert(3)

    val four = tree.insert(4)

    tree.removeNode(tree.search(3).get)
    assert(tree.toList == List(1, 2, 4))
    assert(tree.search(3).isEmpty)

    assert(root.left.contains(one))
    assert(root.right.contains(four))

    val anotherTree = new BinarySearchTree[Int]()
    val anotherRoot = anotherTree.insert(3)
    anotherTree.insert(1)
    val anotherTwo = anotherTree.insert(2)
    val anotherFour = anotherTree.insert(4)

    anotherTree.removeNode(anotherTree.search(1).get)
    assert(anotherTree.toList == List(2, 3, 4))
    assert(anotherTree.search(1).isEmpty)

    assert(anotherRoot.left.contains(anotherTwo))
    assert(anotherRoot.right.contains(anotherFour))
  }

  test("delete replace deleted node with it's successor") {
    val tree = new BinarySearchTree[Int]()
    val root = tree.insert(2)
    tree.insert(1)
    tree.insert(4)
    val three = tree.insert(3)
    val five = tree.insert(5)

    tree.removeNode(tree.search(4).get)

    assert(tree.toList == List(1, 2, 3, 5))
    assert(tree.search(4).isEmpty)

    assert(root.right.contains(five))
    assert(five.parent.contains(root))
    assert(five.right.isEmpty)
    assert(five.left.contains(three))
    assert(three.parent.contains(five))

  }


  test("if we delete all elements the tree must be empty") {
    val tree = new BinarySearchTree[Int]()
    tree.insert(3)
    tree.insert(10)
    tree.insert(5)
    tree.insert(8)
    tree.insert(9)
    tree.insert(1)
    tree.insert(2)
    tree.insert(4)
    tree.insert(7)
    tree.insert(6)

    for (i <- 1 to 10) {
      tree.removeNode(tree.search(i).get)
    }
    assert(tree.toList.isEmpty)
  }


}
