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

  def a = new Rational(12, 30)
  println(a.toString())
  println(a.less(new Rational(1, 0)))
  println(-a)
}

class Rational(x: Int, y: Int) {
  def this(x:Int) = this(x, 1)
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numerator = x
  def denominator = y

  override def toString() = {
    this.numerator / g + "/" + this.denominator/g
  }
  def less(that: Rational) = numerator * that.denominator < that.numerator * denominator
  def max(that: Rational) = if (this.less(that)) that else this

  def + (that: Rational) = {
    new Rational(numerator + that.numerator / denominator + that.denominator)
  }

  def unary_- : Rational = new Rational(-numerator, denominator)
}

