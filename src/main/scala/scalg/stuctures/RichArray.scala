package scalg.stuctures

class RichArray[T] (a: Array[T]) {
  def swap(i: Int, j: Int): Unit = {
    val c = a(i)
    a(i) = a(j)
    a(j) = c
  }
}

object RichArray {
  implicit def makeArrayRich[T](a: Array[T]): RichArray[T] = new RichArray[T](a)

}
