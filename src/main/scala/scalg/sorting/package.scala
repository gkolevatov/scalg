package scalg

import scala.reflect.ClassTag

package object sorting {

  implicit def array2SortableArray[T](a: Array[T])(implicit ordering: Ordering[T], classTag: ClassTag[T]): Orderable[T] = new Orderable(a)

}
