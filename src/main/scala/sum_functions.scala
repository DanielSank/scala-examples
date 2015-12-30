object SumFunctionsMain {

  // Evaluate a function over a range of integers and sum the results.
  def sumFunction(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sumFunction(f, a + 1, b)
  }

  /* Note that the above implementation is not tail recursive. Also not that it
   * is not possible to partially evaluate the function. For example, suppose
   * we have a particular mathematical function f: Int => Int which we would
   * like to evaluate and sum over several different ranges of integers. To do
   * that here, we'd have to do something like this
   * 
   * def square(x: Int): Int = {x*x}
   * 
   * def sumSquares(a: Int, b: Int): Int = {sumFunction(square, a, b)}
   */

  /* This version uses Currying to make partial evaluation easy.
   * 
   * For example, we can construct a function equivalent to the sumSquares
   * described above with less typing:
   * 
   * sumSquares = sumFunction1(square)_
   */
  def sumFunction1(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sumFunction1(f)(a + 1, b)
  }

  // This version is tail recursive.
  def sumFunction2(f: Int => Int, a: Int, b: Int): Int = {
    def sum(x: Int, y: Int, total: Int): Int = {
      if (x <= y) sum(x + 1, y, f(x) + total)
      else total
    }
    sum(a, b, 0)
  }

  // This version is tail recursive and Curried.
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

    println(s"sumFunction: ${sumFunction(square, 5, 8)}")

    val sum1 = sumFunction1(square)_
    println(s"sumFunction1: ${sum1(5, 8)}")

    println(s"sumFunction2: ${sumFunction2(square, 5, 8)}")

    val sum3 = sumFunction3(square)_
    println(s"sumFunction3: ${sum3(5, 8)}")
  }
}
