trait RList[T] {
  def isEmpty: Boolean
  def head: T
  def tail: RList[T]
}

class Cons[T](val head: T, val tail: RList[T]) extends RList[T] {
  def isEmpty = false
}

class Nil[T] extends RList[T] {
  def isEmpty:Boolean =  true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object session4 extends App {
  def nth[T](x: Int, xs: RList[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    if (x == 0) xs.head
    else nth(x - 1, xs.tail)
  }
}

object RList extends {
  // RList(1, 2)
  def apply[T](x1: T, x2: T): RList[T] = new Cons(x1, new Cons(x2, new Nil))
}

