object Exercise2 extends App {
    def balance(chars: List[Char]): Boolean = {
      def loop(char_list: List[Char], open: Int, close: Int): Boolean = {
            if (close > open) false
            else if (char_list.isEmpty) {
                if (open - close == 0) true
                else false
            }
            else if (char_list.head == '(') loop(char_list.tail, open+1, close)
            else if (char_list.head == ')') loop(char_list.tail, open, close+1)
            else loop(char_list.tail, open, close)
        }
        loop(chars, 0, 0)
    }
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
}
