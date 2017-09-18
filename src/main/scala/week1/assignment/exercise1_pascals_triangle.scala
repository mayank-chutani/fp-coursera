
object Exercise1 extends App{
    def pascal(c: Int, r: Int): Int = {
      def loop(col: Int, row: Int): Int = {
            if (col == 0 || col == row) 1
            else loop(col-1, row-1) + loop(col, row-1)
        }
        loop(c, r)
    }
    println(pascal(0,2))
    println(pascal(1,2))
    println(pascal(1,3))
    println(pascal(1, 4))
}