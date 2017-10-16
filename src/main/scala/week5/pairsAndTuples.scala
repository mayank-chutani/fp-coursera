object pairsAndTuples extends App {

  def mergeSort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (List(), ys) => ys
        case (xs, List()) => xs
        case (l :: ls, m :: ms) => if (l < m) l :: merge(ls, ys) else m :: xs
      }
      val (fst, scnd) = xs splitAt n
      merge(mergeSort(fst), mergeSort(scnd))
    }
  }
}
