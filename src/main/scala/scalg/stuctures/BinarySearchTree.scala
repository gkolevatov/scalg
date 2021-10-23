package scalg.stuctures

import scala.collection.mutable

/**
 * Represents binary not balanced search tree.
 * Allows to store elements of the same type check weather the tree contains them or not. The elements should support
 * ordering. All elements could be traversed in ascending order. Tree als
 *
 * It has two interfaces, first is public and is similar to java SortedSet. It contains the next methods
 * <ul>
 * <li>[[scalg.stuctures.BinarySearchTree#contains]] -- True if tree contains an element  </li>
 * <li>[[scalg.stuctures.BinarySearchTree#min]] -- Returns the smallest element of the tree </li>
 * <li>[[scalg.stuctures.BinarySearchTree#max]] -- Returns the biggest element of the tree </li>
 * <li>[[scalg.stuctures.BinarySearchTree#predecessor]] -- Returns the biggest element that is smaller then given element </li>
 * <li>[[scalg.stuctures.BinarySearchTree#successor]] -- Returns the smallest element that is bigger then given element </li>
 * <li>[[scalg.stuctures.BinarySearchTree#+=]] -- alias for add </li>
 * <li>[[scalg.stuctures.BinarySearchTree#add]] -- adds element to the tree </li>
 * <li>[[scalg.stuctures.BinarySearchTree#-=]] -- alias for remove </li>
 * <li>[[scalg.stuctures.BinarySearchTree#remove]] -- removes element from the tree </li>
 * </ul>
 *
 * Second is package-private methods which gives access to tree nodes, and therefore allows to implement more complex
 * algorithms
 * <ul>
 * <li>[[scalg.stuctures.BinarySearchTree#search]] -- Returns the node with the smallest value</li>
 * <li>[[scalg.stuctures.BinarySearchTree#minNode]] -- Returns the node with the smallest value</li>
 * <li>[[scalg.stuctures.BinarySearchTree#maxNode]] -- Returns the node with the biggest value</li>
 * <li>[[scalg.stuctures.BinarySearchTree#predecessor]] -- Returns the node which value is the biggest that is smaller then the value of the given node</li>
 * <li>[[scalg.stuctures.BinarySearchTree#successor]] -- Returns the node which value is the smallest that is bigger then the value of the given node</li>
 * <li>[[scalg.stuctures.BinarySearchTree#insert]] -- Inserts given value into the tree and returns created node </li>
 * <li>[[scalg.stuctures.BinarySearchTree#removeNode]] -- Removes given node from the tree and return it </li>
 * </ul>
 *
 */
class BinarySearchTree[T](implicit ordering: Ordering[T]) extends mutable.Iterable[T] {
  private var root: Option[BSTNode[T]] = None

  /**
   * Checks weather tree contained the element
   * @param element element to check if tree contains it
   * @return True if tree contains the element, false if it does not
   */
  def contains(element: T): Boolean = search(element).isDefined

  /**
   * @return the smallest element of the tree. None if the tree is empty
   */
  def min: Option[T] = minNode.map(_.value)

  /**
   * @return the biggest element of the tree. None if the tree is empty
   */
  def max: Option[T] = maxNode.map(_.value)

  /**
   * Returns the biggest element that is smaller then given element which is its predecessor in ordering.
   * Returns None if the tree doesn't contain the given element
   * Returns None if there's no such elements, for example, if given element is the smallest.
   * @return the give element's predecessor
   */
  def predecessor(element: T): Option[T] = search(element).flatMap(predecessor).map(_.value)

  /**
   * Returns the smallest element that is bigger then given element which is its successor in ordering.
   * Returns None if the tree doesn't contain the given element
   * Returns None if there's no such elements, for example, if given element is the smallest.
   * @return the give element's successor
   */
  def successor(element: T): Option[T] = search(element).flatMap(successor).map(_.value)

  /**
   * Alias for add
   */
  final def +=(element: T): BinarySearchTree[T] = add(element)

  /**
   * Alias for remove
   */
  final def -=(element: T): BinarySearchTree[T] = remove(element)

  /**
   * Adds element to the tree. If the tree already contains the elements, then the method has no effect
   * @param element which will be added
   * @return Tree itself
   */
  def add(element: T): BinarySearchTree[T] = {
    insert(element)
    this
  }

  /**
   * Remove element from the tree. If the tree doesn't contain the elements, then the method has no effect
   * @param element which will be removed
   * @return Tree itself
   */
  def remove(element: T): BinarySearchTree[T] = {
    val node = search(element)
    node.foreach(removeNode)
    this
  }

  /**
   *
   * @param value
   * @return
   */
  private[stuctures] def search(value: T): Option[BSTNode[T]] = {
    var current = root
    while (current.isDefined && !ordering.equiv(current.get.value, value)) {
      current = if (ordering.lt(current.get.value, value)) {
        current.get.right
      } else {
        current.get.left
      }
    }

    current
  }

  private[stuctures] def minNode: Option[BSTNode[T]] = minNode(root)

  private[stuctures] def maxNode: Option[BSTNode[T]] = maxNode(root)

  private[stuctures] def predecessor(node: BSTNode[T]): Option[BSTNode[T]] = {
    if (node.isRemoved) {
      None
    } else {
      node.left match {
        case Some(_) => maxNode(node.left)
        case None =>
          var a = node
          var b = node.parent
          while (b.isDefined && !b.get.right.contains(a)) {
            a = b.get
            b = a.parent
          }

          b
      }
    }
  }

  private[stuctures] def successor(node: BSTNode[T]): Option[BSTNode[T]] = {
    if (node.isRemoved) {
      None
    } else {
      node.right match {
        case Some(_) => minNode(node.right)
        case None =>
          var a = node
          var b = node.parent
          while (b.isDefined && !b.get.left.contains(a)) {
            a = b.get
            b = a.parent
          }

          b
      }
    }
  }

  private[stuctures] def insert(key: T): BSTNode[T] = {
    var current = root
    var previous = root

    while (current.isDefined && !ordering.equiv(current.get.value, key)) {
      previous = current
      current = if (ordering.lt(current.get.value, key)) {
        current.get.right
      } else {
        current.get.left
      }
    }

    if (current.isDefined) {
      current.get
    } else {
      if (previous.isEmpty) {
        root = Some(new BSTNode[T](key, None))
        root.get
      } else {
        val newNode = new BSTNode(key, previous)

        if (ordering.lt(previous.get.value, key)) {
          previous.get.setRight(Some(newNode))
        } else {
          previous.get.setLeft(Some(newNode))
        }

        newNode
      }

    }
  }

  private[stuctures] def removeNode(node: BSTNode[T]): BSTNode[T] = {
    if (!node.isRemoved) {
      if (node.left.isEmpty) {
        transplant(node, node.right)
      } else if (node.right.isEmpty) {
        transplant(node, node.left)
      } else {
        val successor = minNode(node.right).get
        if (!successor.parent.contains(node)) {
          // replace successor with it's right child
          transplant(successor, successor.right)
          successor.setRight(node.right)
          successor.right.get.setParent(Some(successor))
        }

        transplant(node, Some(successor))
        successor.setLeft(node.left)
        successor.left.get.setParent(Some(successor))
      }
    }

    node.remove()
  }


  private def minNode(node: Option[BSTNode[T]]): Option[BSTNode[T]] = {
    var c = node
    while (c.isDefined && c.get.left.isDefined) {
      c = c.get.left
    }

    c
  }

  private def maxNode(node: Option[BSTNode[T]]): Option[BSTNode[T]] = {
    var c = node
    while (c.isDefined && c.get.right.isDefined) {
      c = c.get.right
    }

    c
  }


  private def transplant(dest: BSTNode[T], source: Option[BSTNode[T]]): Unit = {
    if (dest.parent.isEmpty) {
      root = source
    } else {
      if (dest.parent.get.left.contains(dest)) {
        dest.parent.get.setLeft(source)
      } else {
        dest.parent.get.setRight(source)
      }
    }

    source.foreach(_.setParent(dest.parent))
  }


  override def iterator: Iterator[T] = {
    new Iterator[T] {
      private val stack = mutable.Stack[BSTNode[T]]()
      addNode(root)

      def addNode(node: Option[BSTNode[T]]): Unit = {
        var i = node
        while (i.isDefined) {
          stack.push(i.get)
          i = i.get.left
        }
      }

      override def hasNext: Boolean = stack.nonEmpty

      override def next(): T = {
        val node = stack.pop()
        addNode(node.right)
        node.value
      }
    }
  }

  class BSTNode[T](val value: T, parentNode: Option[BSTNode[T]])(implicit ordering: Ordering[T]) {
    private var _parent = parentNode
    private var _left: Option[BSTNode[T]] = None
    private var _right: Option[BSTNode[T]] = None
    private var _isRemoved: Boolean = false

    def left: Option[BSTNode[T]] = _left

    def right: Option[BSTNode[T]] = _right

    def parent: Option[BSTNode[T]] = _parent

    private[BinarySearchTree] def isRemoved: Boolean = _isRemoved

    private[BinarySearchTree] def setParent(node: Option[BSTNode[T]]): Option[BSTNode[T]] = {
      _parent = node
      _parent
    }

    private[BinarySearchTree] def setRight(node: Option[BSTNode[T]]): Option[BSTNode[T]] = {
      _right = node
      _right
    }

    private[BinarySearchTree] def setLeft(node: Option[BSTNode[T]]): Option[BSTNode[T]] = {
      _left = node
      _left
    }

    private[BinarySearchTree] def remove(): BSTNode[T] = {
      _parent = None
      _left = None
      _right = None
      _isRemoved = true

      this
    }
  }

}
