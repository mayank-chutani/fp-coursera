object session2 extends App {
  def sum(f: Int => Int, a: Int, b: Int): Int = 
    if (a > b) 0
    else f(a) + sum(f, a+1, b)

    println(sum(x => x * x, 1, 4))
}

