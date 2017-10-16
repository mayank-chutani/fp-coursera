object ListPatterMatching extends App {

  def last[T](l: List[T]): T = l match {
    case Nil => throw new Error("empty list")
    case x :: Nil => x
    case x :: xs => last(xs)
  }

  def init[T](l: List[T]): List[T] = l match {
    case Nil => throw new Error("init of empty list")
    case x :: Nil => List()
    case x :: xs => x :: init(xs)
  }

  def concat[T](l1: List[T], l2: List[T]): List[T] = l1 match {
    case List() => l2
    case y :: ys => y :: concat(ys, l2)
  }

  def reverse[T](l: List[T]): List[T] = l match {
    case List() => l
    case y :: ys => reverse(ys) ::: List(y)
  }

  def removeAt[T](l: List[T], n: Int): List[T] = (l take n) ::: (l drop n + 1)
}
