object MainPattern {
  abstract class Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(a, b) => eval(a) + eval(b)
  }
  def main(args: Array[String]): Unit = {
    val e1 = new Number(5)
    val e2 = new Number(6)
    val e3 = new Number(8)
    val exp1 = new Sum(e1, e2)
    val exp2 = new Sum(exp1, e3)
    println(s"result: ${eval(exp2)}")
  }
}
