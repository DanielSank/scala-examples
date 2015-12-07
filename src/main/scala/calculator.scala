object MyProgram {

  // Partial (curried)
  def sumFunction1(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sumFunction1(f)(a + 1, b)
  }

  // Tail recursive
  def sumFunction2(f: Int => Int): (Int, Int, Int) => Int = {
    def sum(a: Int, b: Int, total: Int): Int = {
      if (a <= b) sum(a+1, b, f(a) + total)
      else total
    }
    sum
  }

  // Tail recursive and partial (curried)
  def sumFunction3(f: Int => Int)(a: Int, b: Int): Int = {
    def recur(total: Int)(f: Int => Int)(a: Int, b: Int): Int = {
        if (a <= b) recur(f(a) + total)(f)(a+1, b)
        else total
    }
    recur(0)(f)(a, b)
  }

  def main(args: Array[String]) {
    def square(x: Int): Int = {x*x}
    println("Sum of squares from 5 to 8 = 174")

    val sum1 = sumFunction1(square)_
    println(s"sumFunction1: ${sum1(5, 8)}")

    println(s"sumFunction2: ${sumFunction2(square)(5, 8, 0)}")

    val sum3 = sumFunction3(square)_
    println(s"sumFunction3: ${sum3(5, 8)}")
  }
}
