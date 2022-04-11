package scalg.stuctures.trees

import Color._

import scala.annotation.tailrec


class RedBlackTree[T](implicit ordering: Ordering[T])  {

  private var root: Option[RBTNode] = None

  case class RBTNode(    value: T,
                     var color: Color,
                     var parent: Option[RBTNode],
                     var left: Option[RBTNode] = None,
                     var right: Option[RBTNode] = None)
                    (implicit ordering: Ordering[T])

  private def leftRotate(node: RBTNode): Unit = {
    val y = node.right.get
    // assign y left to x right
    node.right = y.left
    if (node.right.isDefined) {
      node.right.get.parent = Some(node)
    }
    // assign y left to x
    y.left = Some(node)

    node.parent match {
      case Some(parentNode) =>
        if (parentNode.left.contains(node)) {
          parentNode.left = Some(y)
        } else {
          parentNode.right = Some(y)
        }

      case None => {
        root = Some(y)
      }
    }

    node.parent = Some(y)
    y.left = Some(node)

  }

  private def rightRotate(node: RBTNode): Unit = {
    val x = node.left.get
    val y = node
    // assign x right to y left
    y.left = x.right
    x.right.foreach(_.parent = Some(y))

    // assign x left to y
    y.parent match {
      case Some(yParent) if (yParent.left.contains(y)) => yParent.left = Some(x)
      case Some(yParent) if (yParent.right.contains(y)) => yParent.right = Some(x)
      case None => root = Some(x)
    }

    x.right = Some(y)
    y.parent = Some(x)

  }


  private def search(key: T): Option[RBTNode] = root.map(searchFrom(key, _))

  @tailrec
  private def searchFrom(key: T, node: RBTNode): RBTNode = {
    if (node.value == key)
      node
    else {
      (if (ordering.lt(key, node.value)) node.left else node.right) match {
        case Some(nextNode) => searchFrom(key, nextNode)
        case None => node
      }
    }
  }

  private def fixup(node: RBTNode): Unit = {
    var x = node

    while (x.parent.exists(_.color == Color.Red)) {
      // Grandparent exists because parent is RED
      val grand = x.parent.get.parent.get
      if (x.parent == grand.left) {
        val uncle = x.parent.get.parent.get.right
        // first case uncle is Red
        if (uncle.exists(_.color == Color.Red)) {
          uncle.foreach(_.color = Color.Black)
          x.parent.get.color = Color.Black
          x = grand
        } else {
          if (x.parent.get.right.contains(x)) {
            x = x.parent.get
            leftRotate(x)
          }

          x.parent.get.color = Color.Black
          grand.color = Color.Red
          rightRotate(grand)
        }
      } else {
        val uncle = x.parent.get.parent.get.left

        // first case uncle is Red
        if (uncle.exists(_.color == Color.Red)) {
          uncle.foreach(_.color = Color.Black)
          x.parent.get.color = Color.Black
          x = grand
        } else {
          if (x.parent.get.left.contains(x)) {
            x = x.parent.get
            rightRotate(x)
          }

          x.parent.get.color = Color.Black
          grand.color = Color.Red
          leftRotate(grand)
        }
      }
    }
  }

  def insert(key: T): Boolean = {
    val target = search(key)
    if (target.exists(_.value == key)) {
      false
    } else {

      val newNode = Some(RBTNode(key, Color.Red, target))

      target match {
        case Some(node) if ordering.lt(key, target.get.value) => node.left = newNode
        case Some(node) => node.right = newNode
        case None => root = newNode
      }

      fixup(newNode.get)

      true
    }
  }







}


