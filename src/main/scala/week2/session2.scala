object session2 extends App {
  def sum(f: Int => Int, a: Int, b: Int): Int = 
    if (a > b) 0
    else f(a) + sum(f, a+1, b)

  def sumTailRecursive(f: Int => Int)(a: Int, b: Int): Int = { def loop(acc: Int, x: Int, y: Int): Int = { if (x > y) acc
      else {
        loop(acc + f(x), x + 1, y)
      }
    }
    loop(0, a, b)
  }
  println(sumTailRecursive(x => x * x)(1, 4))

    println(sum(x => x * x, 1, 4))

    def mapReduce(f: Int => Int)(combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int= {
      if (a > b) zero
      else combine(f(a), mapReduce(f)(combine, zero)(a+1, b))
    }
    println(mapReduce(x => x * x)((x, y) => x + y, 0)(1, 4))
}
