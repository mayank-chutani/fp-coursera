object TestObj extends App {
    def tailRecursiveFactorial(num: Integer) = {
        def loop(x: Integer, acc: Integer): Integer = {
            if (x == 1) {
                acc
            }
            else {
                loop(x-1, acc*x)
            }
        }
        loop(num, 1)
    }
    println(tailRecursiveFactorial(5))
    
}