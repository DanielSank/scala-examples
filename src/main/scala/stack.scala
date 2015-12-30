abstract class Stack[+A] {
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
  def isPrefix[B >: A](other: Stack[B]): Boolean = {
    def recur(a: Stack[A], b: Stack[B]): Boolean = {
      a.isEmpty ||
      a.top == b.top && recur(a.pop, b.pop)
    }
    recur(this, other)
  }
}

case object EmptyStack extends Stack[Nothing] {
  def push[A](x: A): NonEmptyStack[A] = new NonEmptyStack[A](x, this)
  def isEmpty = true
  def top = error("EmptyStack.top")
  def pop = error("EmptyStack.pop")
}

case class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A] {
  def push[B <% A](x: B): Stack[A] = new NonEmptyStack[A](x, this)
  def isEmpty = false
  def top = elem
  def pop = rest
}

object StackMain {
  def main(args: Array[String]) = {
    val myStack = EmptyStack.push(54.6)
    val x = 4
    val stack1 = myStack.push(x)
    println(stack1.top)
  }
}
