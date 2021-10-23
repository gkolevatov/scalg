package scalg.stuctures

import scalg.stuctures

import scala.collection.mutable

/**
 * Represents tree structure
 * @tparam T type of values that tree node stores
 */
trait TreeNode[T] extends Iterable[T] {

  /**
   * Creates child node of specified index, assigns value to the child node.
   * If child node already exists it will be rewritten with the new node.
   * Throws IllegalArgumentException if index is invalid
   *
   * @param childIndex index of the child node
   * @param childValue value to be assigned to the child node
   * @throws java.lang.IllegalArgumentException if child index is invalid
   * @return created tree node
   */
  @throws(classOf[IllegalArgumentException])
  def update(childIndex: Int, childValue: T): TreeNode[T]

  /**
   * Returns child node of specified index
   * @param childIndex index of the child node
   * @throws java.lang.IllegalArgumentException  if child index is invalid
   * @return child node of specified index
   */
  @throws(classOf[IllegalArgumentException])
  def apply(childIndex: Int): Option[TreeNode[T]]

  /**
   * @return value assigned to the node
   */
  def value: T
}

abstract class AbstractTreeNode[T](nodeValue: T) extends TreeNode[T] {
  override def value: T = nodeValue
}

class FixedChildTree[T](nodeValue: T, childrenCount: Int) extends AbstractTreeNode[T](nodeValue) {

  private val children = new Array[FixedChildTree[T]](childrenCount)

  protected def createChild(childValue: T): FixedChildTree[T] = new FixedChildTree[T](childValue, childrenCount)

  override def update(childIndex: Int, childValue: T): TreeNode[T] = {
    children(childIndex) = createChild(value)
    children(childIndex)
  }

  def apply(childIndex: Int): Option[TreeNode[T]] = Option(children(childIndex))

  override def iterator: Iterator[T] = {
    val stack: mutable.Stack[FixedChildTree[T]] = new mutable.Stack()

    stack.push(this);

    new Iterator[T] {
      override def hasNext: Boolean = stack.nonEmpty

      override def next(): T = {
        if (stack.nonEmpty) {
          val current = stack.pop()

          for (child <- current.children if child != null) {
            stack.push(child)
          }

          current.value
        } else {
          throw new NoSuchElementException();
        }
      }
    }
  }
}

class BinaryTree[T](value: T) extends FixedChildTree[T](value, 2) {

  override def createChild(childValue: T): FixedChildTree[T] = new BinaryTree[T](childValue)

  def left(value: T): TreeNode[T] = update(0, value).asInstanceOf[stuctures.BinaryTree[T]]

  def right(value: T): TreeNode[T] = update(1, value).asInstanceOf[stuctures.BinaryTree[T]]

  def left: Option[TreeNode[T]] = apply(0).map(_.asInstanceOf[stuctures.BinaryTree[T]])
  def right: Option[TreeNode[T]] = apply(1).map(_.asInstanceOf[stuctures.BinaryTree[T]])
}
