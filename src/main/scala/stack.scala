class Fruit
class Apple extends Fruit
class Gala extends Apple
class Banana extends Fruit

abstract class Stack[+A] {
  def push[B >: A](x: B): Stack[B] = new NonEmptyStack[B](x, this)
  def top: A
  def pop: Stack[A]
  def isPrefix[B >: A](other: Stack[B]): Boolean = {
    def recur(a: Stack[A], b: Stack[B]): Boolean = a match {
      case EmptyStack => true
      case NonEmptyStack(top, rest) => top == b.top && recur(a.pop, b.pop)
    }
    recur(this, other)
  }
}

case object EmptyStack extends Stack[Nothing] {
  def top = error("EmptyStack.top")
  def pop = error("EmptyStack.pop")
}

case class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A] {
  def top = elem
  def pop = rest
}

object StackMain {
  def main(args: Array[String]) = {
    println("This example doesn't actually do anything.")
    println("Look at the source code to learn some advanced variance")
    println("and type parameter bound techniques.")

    val apple = new Apple
    val gala = new Gala
    val banana = new Banana

    val stack1 = EmptyStack.push(apple)
    val stack2 = stack1.push(gala)
    val stack3 = stack2.push(banana)
  }
}
